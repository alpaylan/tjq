use z3::{
    ast::{self, Ast, Bool, Datatype, Float, Int},
    datatype_builder::create_datatypes,
    DatatypeAccessor, DatatypeBuilder, RecFuncDecl, Sort,
};

use crate::{
    experimental_type_inference::{Constraint, Equality, Relation},
    Shape, Subtyping,
};

pub struct Enc {
    shape: z3::DatatypeSort,
    list_shape: z3::DatatypeSort,
    list_field: z3::DatatypeSort,
    maybe_bool: z3::DatatypeSort,
    maybe_float: z3::DatatypeSort,
    maybe_int: z3::DatatypeSort,
    maybe_str: z3::DatatypeSort,
    field: z3::DatatypeSort,
    str: Sort,
    // Subtype : Shape × Shape → Bool
    subtype: RecFuncDecl,

    // helpers (declare later in `declare_rec_helpers`)
    pub tup_leq: Option<z3::RecFuncDecl>,
    pub has_field: Option<z3::RecFuncDecl>,
    pub obj_leq: Option<z3::RecFuncDecl>,
}

// ---------- Mutually-recursive group: Shape, ListShape, Field, ListField ----------
const IDX_TOP: usize = 0;
const IDX_BOT: usize = 1;
const IDX_NULL: usize = 2;
const IDX_BOOL: usize = 3;
const IDX_NUM: usize = 4;
const IDX_STR: usize = 5;
const IDX_ARR: usize = 6;
const IDX_TUP: usize = 7;
const IDX_OBJ: usize = 8;
const IDX_TVAR: usize = 9;

impl Enc {
    pub fn new() -> Self {
        // ---------- Non-recursive ADTs: Maybe* ----------
        let maybe_bool = DatatypeBuilder::new("MaybeBool")
            .variant("MBNone", vec![])
            .variant(
                "MBSome",
                vec![("val", DatatypeAccessor::Sort(Sort::bool()))],
            )
            .finish();

        let maybe_float = DatatypeBuilder::new("MaybeReal")
            .variant("MRNone", vec![])
            .variant(
                "MRSome",
                vec![("val", DatatypeAccessor::Sort(Sort::double()))],
            )
            .finish();

        let maybe_int = DatatypeBuilder::new("MaybeInt")
            .variant("MINone", vec![])
            .variant("MISome", vec![("val", DatatypeAccessor::Sort(Sort::int()))])
            .finish();

        let maybe_str = DatatypeBuilder::new("MaybeStr")
            .variant("MSNone", vec![])
            .variant(
                "MSSome",
                vec![("val", DatatypeAccessor::Sort(Sort::string()))],
            )
            .finish();

        let b_shape = DatatypeBuilder::new("Shape")
            .variant("Top", vec![])
            .variant("Bot", vec![])
            .variant("Null", vec![])
            .variant(
                "Bool",
                vec![("opt", DatatypeAccessor::Sort(maybe_bool.sort.clone()))],
            )
            .variant(
                "Num",
                vec![("opt", DatatypeAccessor::Sort(maybe_float.sort.clone()))],
            )
            .variant(
                "Str",
                vec![("opt", DatatypeAccessor::Sort(maybe_str.sort.clone()))],
            )
            .variant(
                "Arr",
                vec![
                    ("elem", DatatypeAccessor::Datatype("Shape".into())),
                    ("len", DatatypeAccessor::Sort(maybe_int.sort.clone())),
                ],
            )
            .variant(
                "Tup",
                vec![("elts", DatatypeAccessor::Datatype("ListShape".into()))],
            )
            .variant(
                "Obj",
                vec![("fields", DatatypeAccessor::Datatype("ListField".into()))],
            )
            .variant("TVar", vec![("id", DatatypeAccessor::Sort(Sort::int()))]);

        let b_list_shape = DatatypeBuilder::new("ListShape")
            .variant("LSNil", vec![])
            .variant(
                "LSCons",
                vec![
                    ("head", DatatypeAccessor::Datatype("Shape".into())),
                    ("tail", DatatypeAccessor::Datatype("ListShape".into())),
                ],
            );

        let b_field = DatatypeBuilder::new("Field").variant(
            "MkField",
            vec![
                ("name", DatatypeAccessor::Sort(Sort::string())),
                ("ty", DatatypeAccessor::Datatype("Shape".into())),
            ],
        );

        let b_list_field = DatatypeBuilder::new("ListField")
            .variant("LFNil", vec![])
            .variant(
                "LFCons",
                vec![
                    ("head", DatatypeAccessor::Datatype("Field".into())),
                    ("tail", DatatypeAccessor::Datatype("ListField".into())),
                ],
            );

        // Finish ALL four at once — then MOVE them out (no .clone()).
        let mut sorts_iter =
            create_datatypes(vec![b_shape, b_list_shape, b_field, b_list_field]).into_iter();
        let shape = sorts_iter.next().unwrap();
        let list_shape = sorts_iter.next().unwrap();
        let field = sorts_iter.next().unwrap();
        let list_field = sorts_iter.next().unwrap();

        // ---------- Recursive predicates with CORRECT signatures ----------
        let subtype = RecFuncDecl::new("Subtype", &[&shape.sort, &shape.sort], &Sort::bool());
        let tup_leq = RecFuncDecl::new(
            "TupLeq",
            &[&list_shape.sort, &list_shape.sort],
            &Sort::bool(),
        );
        let has_field = RecFuncDecl::new(
            "HasField",
            &[&list_field.sort, &Sort::string(), &shape.sort],
            &Sort::bool(),
        );
        let obj_leq = RecFuncDecl::new(
            "ObjLeq",
            &[&list_field.sort, &list_field.sort],
            &Sort::bool(),
        );

        Self {
            // sorts
            shape,
            list_shape,
            field,
            list_field,
            maybe_bool,
            maybe_float,
            maybe_int,
            maybe_str,
            str: Sort::string(),
            // if your struct stores more accessors/constructors, grab them here similarly…

            // recursive functions
            subtype,
            tup_leq: Some(tup_leq),
            has_field: Some(has_field),
            obj_leq: Some(obj_leq),
        }
    }

    pub fn shape_const(&self, name: &str) -> Datatype {
        Datatype::new_const(name, &self.shape.sort)
    }

    fn top(&self) -> Datatype {
        self.shape.variants[IDX_TOP as usize]
            .constructor
            .apply(&[])
            .as_datatype()
            .unwrap()
    }
    fn is_top(&self, v: &Datatype) -> Bool {
        self.shape.variants[IDX_TOP as usize]
            .tester
            .apply(&[v])
            .as_bool()
            .unwrap()
    }

    fn bot(&self) -> Datatype {
        self.shape.variants[IDX_BOT as usize]
            .constructor
            .apply(&[])
            .as_datatype()
            .unwrap()
    }
    fn is_bot(&self, v: &Datatype) -> Bool {
        self.shape.variants[IDX_BOT as usize]
            .tester
            .apply(&[v])
            .as_bool()
            .unwrap()
    }

    fn null(&self) -> Datatype {
        self.shape.variants[IDX_NULL as usize]
            .constructor
            .apply(&[])
            .as_datatype()
            .unwrap()
    }
    fn is_null(&self, v: &Datatype) -> Bool {
        self.shape.variants[IDX_NULL as usize]
            .tester
            .apply(&[v])
            .as_bool()
            .unwrap()
    }

    pub fn bool(&self, b: bool) -> Datatype {
        let inner = self.maybe_bool.variants[1]
            .constructor
            .apply(&[&Bool::from(b)])
            .as_datatype()
            .unwrap();
        let outer = self.shape.variants[IDX_BOOL as usize]
            .constructor
            .apply(&[&inner])
            .as_datatype()
            .unwrap();
        outer
    }

    pub fn bool_(&self) -> Datatype {
        let inner = self.maybe_bool.variants[0]
            .constructor
            .apply(&[])
            .as_datatype()
            .unwrap();

        let outer = self.shape.variants[IDX_BOOL as usize]
            .constructor
            .apply(&[&inner])
            .as_datatype()
            .unwrap();
        outer
    }

    fn is_bool(&self, v: &Datatype) -> Bool {
        self.shape.variants[IDX_BOOL as usize]
            .tester
            .apply(&[v])
            .as_bool()
            .unwrap()
    }

    fn num(&self, n: f64) -> Datatype {
        let inner = self.maybe_float.variants[1]
            .constructor
            .apply(&[&Float::from_f64(n)])
            .as_datatype()
            .unwrap();
        let outer = self.shape.variants[IDX_NUM as usize]
            .constructor
            .apply(&[&inner])
            .as_datatype()
            .unwrap();
        outer
    }
    fn num_(&self) -> Datatype {
        let inner = self.maybe_float.variants[0]
            .constructor
            .apply(&[])
            .as_datatype()
            .unwrap();
        let outer = self.shape.variants[IDX_NUM as usize]
            .constructor
            .apply(&[&inner])
            .as_datatype()
            .unwrap();
        outer
    }
    fn is_num(&self, v: &Datatype) -> Bool {
        self.shape.variants[IDX_NUM as usize]
            .tester
            .apply(&[v])
            .as_bool()
            .unwrap()
    }

    fn str(&self, t: &str) -> Datatype {
        // Construct MSSome(<String>)
        let inner = self.maybe_str.variants[1]
            .constructor
            .apply(&[&ast::String::from(t)])
            .as_datatype()
            .unwrap();
        // Construct Shape(MSSome(<String>))
        let outer = self.shape.variants[IDX_STR as usize]
            .constructor
            .apply(&[&inner])
            .as_datatype()
            .unwrap();
        outer
    }
    fn str_(&self) -> Datatype {
        let inner = self.maybe_str.variants[0]
            .constructor
            .apply(&[])
            .as_datatype()
            .unwrap();
        let outer = self.shape.variants[IDX_STR as usize]
            .constructor
            .apply(&[&inner])
            .as_datatype()
            .unwrap();
        outer
    }
    fn is_str(&self, v: &Datatype) -> Bool {
        self.shape.variants[IDX_STR as usize]
            .tester
            .apply(&[v])
            .as_bool()
            .unwrap()
    }

    fn array(&self, e: &Datatype, ml: &Datatype) -> Datatype {
        assert_eq!(e.get_sort(), self.shape.sort);
        assert_eq!(ml.get_sort(), self.maybe_int.sort);
        self.shape.variants[IDX_ARR as usize]
            .constructor
            .apply(&[e, ml])
            .as_datatype()
            .unwrap()
    }
    fn array_(&self, e: &Datatype) -> Datatype {
        let ml = self.maybe_int.variants[0]
            .constructor
            .apply(&[])
            .as_datatype()
            .unwrap();
        self.shape.variants[IDX_ARR as usize]
            .constructor
            .apply(&[e, &ml])
            .as_datatype()
            .unwrap()
    }
    fn is_array(&self, v: &Datatype) -> Bool {
        self.shape.variants[IDX_ARR as usize]
            .tester
            .apply(&[v])
            .as_bool()
            .unwrap()
    }

    fn tuple(&self, elts: &Datatype) -> Datatype {
        self.shape.variants[IDX_TUP as usize]
            .constructor
            .apply(&[elts])
            .as_datatype()
            .unwrap()
    }
    fn is_tuple(&self, v: &Datatype) -> Bool {
        self.shape.variants[IDX_TUP as usize]
            .tester
            .apply(&[v])
            .as_bool()
            .unwrap()
    }

    fn object(&self, fields: &Datatype) -> Datatype {
        self.shape.variants[IDX_OBJ as usize]
            .constructor
            .apply(&[fields])
            .as_datatype()
            .unwrap()
    }
    fn is_obj(&self, v: &Datatype) -> Bool {
        self.shape.variants[IDX_OBJ as usize]
            .tester
            .apply(&[v])
            .as_bool()
            .unwrap()
    }

    fn tvar(&self, id: &Int) -> Datatype {
        self.shape.variants[IDX_TVAR as usize]
            .constructor
            .apply(&[id])
            .as_datatype()
            .unwrap()
    }
    fn is_tvar(&self, v: &Datatype) -> Bool {
        self.shape.variants[IDX_TVAR as usize]
            .tester
            .apply(&[v])
            .as_bool()
            .unwrap()
    }
}

impl Enc {
    pub fn define_subtype(&self) {
        // x, y : Shape
        let x = Datatype::new_const("x", &self.shape.sort);
        let y = Datatype::new_const("y", &self.shape.sort);

        // --- Top/Bottom/Null cases
        let is_bot_x = self.shape.variants[IDX_BOT]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_top_y = self.shape.variants[IDX_TOP]
            .tester
            .apply(&[&y])
            .as_bool()
            .unwrap();

        let is_null_x = self.shape.variants[IDX_NULL]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_null_y = self.shape.variants[IDX_NULL]
            .tester
            .apply(&[&y])
            .as_bool()
            .unwrap();

        // --- Bool refinements
        let is_bool_x = self.shape.variants[IDX_BOOL]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_bool_y = self.shape.variants[IDX_BOOL]
            .tester
            .apply(&[&y])
            .as_bool()
            .unwrap();
        let bopt_x = self.shape.variants[IDX_BOOL].accessors[0]
            .apply(&[&x])
            .as_datatype()
            .unwrap(); // MaybeBool
        let bopt_y = self.shape.variants[IDX_BOOL].accessors[0]
            .apply(&[&y])
            .as_datatype()
            .unwrap();

        // --- Num refinements
        let is_num_x = self.shape.variants[IDX_NUM]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_num_y = self.shape.variants[IDX_NUM]
            .tester
            .apply(&[&y])
            .as_bool()
            .unwrap();
        let nopt_x = self.shape.variants[IDX_NUM].accessors[0]
            .apply(&[&x])
            .as_datatype()
            .unwrap(); // MaybeReal
        let nopt_y = self.shape.variants[IDX_NUM].accessors[0]
            .apply(&[&y])
            .as_datatype()
            .unwrap();

        // --- Str refinements
        let is_str_x = self.shape.variants[IDX_STR]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_str_y = self.shape.variants[IDX_STR]
            .tester
            .apply(&[&y])
            .as_bool()
            .unwrap();
        let sopt_x = self.shape.variants[IDX_STR].accessors[0]
            .apply(&[&x])
            .as_datatype()
            .unwrap(); // MaybeStr
        let sopt_y = self.shape.variants[IDX_STR].accessors[0]
            .apply(&[&y])
            .as_datatype()
            .unwrap();

        // --- Arrays
        let is_arr_x = self.shape.variants[IDX_ARR]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_arr_y = self.shape.variants[IDX_ARR]
            .tester
            .apply(&[&y])
            .as_bool()
            .unwrap();
        let elem_x = self.shape.variants[IDX_ARR].accessors[0]
            .apply(&[&x])
            .as_datatype()
            .unwrap(); // Shape
        let len_x = self.shape.variants[IDX_ARR].accessors[1]
            .apply(&[&x])
            .as_datatype()
            .unwrap(); // MaybeInt
        let elem_y = self.shape.variants[IDX_ARR].accessors[0]
            .apply(&[&y])
            .as_datatype()
            .unwrap();
        let len_y = self.shape.variants[IDX_ARR].accessors[1]
            .apply(&[&y])
            .as_datatype()
            .unwrap();

        // --- Tuples
        let is_tup_x = self.shape.variants[IDX_TUP]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_tup_y = self.shape.variants[IDX_TUP]
            .tester
            .apply(&[&y])
            .as_bool()
            .unwrap();
        let elts_x = self.shape.variants[IDX_TUP].accessors[0]
            .apply(&[&x])
            .as_datatype()
            .unwrap(); // ListShape
        let elts_y = self.shape.variants[IDX_TUP].accessors[0]
            .apply(&[&y])
            .as_datatype()
            .unwrap();

        // --- Objects
        let is_obj_x = self.shape.variants[IDX_OBJ]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_obj_y = self.shape.variants[IDX_OBJ]
            .tester
            .apply(&[&y])
            .as_bool()
            .unwrap();
        let flds_x = self.shape.variants[IDX_OBJ].accessors[0]
            .apply(&[&x])
            .as_datatype()
            .unwrap(); // ListField
        let flds_y = self.shape.variants[IDX_OBJ].accessors[0]
            .apply(&[&y])
            .as_datatype()
            .unwrap();

        // pull helper rec-funcs (unwrap if you stored as Option)
        let tup_leq = self.tup_leq.as_ref().expect("declare_rec_helpers() first");
        let obj_leq = self.obj_leq.as_ref().expect("declare_rec_helpers() first");

        // --- Recursive calls / helper invocations
        let sub_elem = self.subtype.apply(&[&elem_x, &elem_y]).as_bool().unwrap();
        let sub_tup = tup_leq.apply(&[&elts_x, &elts_y]).as_bool().unwrap();
        let sub_obj = obj_leq.apply(&[&flds_x, &flds_y]).as_bool().unwrap();

        // --- Cases -------------------------------------------------------
        // 1) Bot ≤ anything
        let case_bot = is_bot_x;
        // 2) anything ≤ Top
        let case_top = is_top_y;
        // 3) Null ≤ Null (treat null as its own atom)
        let case_null = Bool::and(&[&is_null_x, &is_null_y]);

        // 4) Bool refinement
        let case_bool = Bool::and(&[
            &is_bool_x,
            &is_bool_y,
            &self.maybe_bool_leq(&bopt_x, &bopt_y),
        ]);

        // 5) Num refinement
        let case_num = Bool::and(&[
            &is_num_x,
            &is_num_y,
            &self.maybe_float_leq(&nopt_x, &nopt_y),
        ]);

        // 6) Str refinement
        let case_str = Bool::and(&[&is_str_x, &is_str_y, &self.maybe_str_leq(&sopt_x, &sopt_y)]);

        // 7) Arrays: element-wise ≤ and length refinement ≤
        let case_arr = Bool::and(&[
            &is_arr_x,
            &is_arr_y,
            &sub_elem,
            &self.maybe_int_leq_math(&len_x, &len_y),
        ]);

        // 8) Tuples: same length + element-wise ≤ (handled by TupLeq)
        let case_tup = Bool::and(&[&is_tup_x, &is_tup_y, &sub_tup]);

        // 9) Objects: width+depth subtyping (handled by ObjLeq)
        let case_obj = Bool::and(&[&is_obj_x, &is_obj_y, &sub_obj]);

        // Or all cases together
        let body = Bool::or(&[
            &case_bot, &case_top, &case_null, &case_bool, &case_num, &case_str, &case_arr,
            &case_tup, &case_obj,
        ]);
        println!("Subtype body: {}", body);

        // Add recursive definition: Subtype(x,y) := body
        self.subtype.add_def(&[&x, &y], &body);
    }
}

impl Enc {
    pub fn maybe_bool_leq(&self, a: &z3::ast::Datatype, b: &z3::ast::Datatype) -> z3::ast::Bool {
        let a_none = self.maybe_bool.variants[0]
            .tester
            .apply(&[a])
            .as_bool()
            .unwrap();
        let a_some = self.maybe_bool.variants[1]
            .tester
            .apply(&[a])
            .as_bool()
            .unwrap();
        let b_none = self.maybe_bool.variants[0]
            .tester
            .apply(&[b])
            .as_bool()
            .unwrap();
        let b_some = self.maybe_bool.variants[1]
            .tester
            .apply(&[b])
            .as_bool()
            .unwrap();
        let aval = self.maybe_bool.variants[1].accessors[0]
            .apply(&[a])
            .as_bool()
            .unwrap();
        let bval = self.maybe_bool.variants[1].accessors[0]
            .apply(&[b])
            .as_bool()
            .unwrap();
        z3::ast::Bool::or(&[
            z3::ast::Bool::and(&[&a_some, &b_some, &aval._eq(&bval)]),
            z3::ast::Bool::and(&[&a_some, &b_none]),
            z3::ast::Bool::and(&[&a_none, &b_none]),
        ])
    }

    pub fn maybe_float_leq(&self, a: &z3::ast::Datatype, b: &z3::ast::Datatype) -> z3::ast::Bool {
        let a_none = self.maybe_float.variants[0]
            .tester
            .apply(&[a])
            .as_bool()
            .unwrap();
        let a_some = self.maybe_float.variants[1]
            .tester
            .apply(&[a])
            .as_bool()
            .unwrap();
        let b_none = self.maybe_float.variants[0]
            .tester
            .apply(&[b])
            .as_bool()
            .unwrap();
        let b_some = self.maybe_float.variants[1]
            .tester
            .apply(&[b])
            .as_bool()
            .unwrap();

        let aval = self.maybe_float.variants[1].accessors[0]
            .apply(&[a])
            .as_float()
            .unwrap();
        let bval = self.maybe_float.variants[1].accessors[0]
            .apply(&[b])
            .as_float()
            .unwrap();

        z3::ast::Bool::or(&[
            z3::ast::Bool::and(&[&a_some, &b_some, &aval._eq(&bval)]),
            z3::ast::Bool::and(&[&a_some, &b_none]),
            z3::ast::Bool::and(&[&a_none, &b_none]),
        ])
    }

    pub fn maybe_int_leq(&self, a: &z3::ast::Datatype, b: &z3::ast::Datatype) -> z3::ast::Bool {
        let a_none = self.maybe_int.variants[0]
            .tester
            .apply(&[a])
            .as_bool()
            .unwrap();
        let a_some = self.maybe_int.variants[1]
            .tester
            .apply(&[a])
            .as_bool()
            .unwrap();
        let b_none = self.maybe_int.variants[0]
            .tester
            .apply(&[b])
            .as_bool()
            .unwrap();
        let b_some = self.maybe_int.variants[1]
            .tester
            .apply(&[b])
            .as_bool()
            .unwrap();

        let aval = self.maybe_int.variants[1].accessors[0]
            .apply(&[a])
            .as_int()
            .unwrap();
        let bval = self.maybe_int.variants[1].accessors[0]
            .apply(&[b])
            .as_int()
            .unwrap();

        z3::ast::Bool::or(&[
            z3::ast::Bool::and(&[&a_some, &b_some, &aval._eq(&bval)]),
            z3::ast::Bool::and(&[&a_some, &b_none]),
            z3::ast::Bool::and(&[&a_none, &b_none]),
        ])
    }

    pub fn maybe_int_leq_math(
        &self,
        a: &z3::ast::Datatype,
        b: &z3::ast::Datatype,
    ) -> z3::ast::Bool {
        let a_none = self.maybe_int.variants[0]
            .tester
            .apply(&[a])
            .as_bool()
            .unwrap();
        let a_some = self.maybe_int.variants[1]
            .tester
            .apply(&[a])
            .as_bool()
            .unwrap();
        let b_none = self.maybe_int.variants[0]
            .tester
            .apply(&[b])
            .as_bool()
            .unwrap();
        let b_some = self.maybe_int.variants[1]
            .tester
            .apply(&[b])
            .as_bool()
            .unwrap();

        let aval = self.maybe_int.variants[1].accessors[0]
            .apply(&[a])
            .as_int()
            .unwrap();
        let bval = self.maybe_int.variants[1].accessors[0]
            .apply(&[b])
            .as_int()
            .unwrap();

        z3::ast::Bool::or(&[
            z3::ast::Bool::and(&[&a_some, &b_some, &aval.ge(&bval)]),
            z3::ast::Bool::and(&[&a_some, &b_none]),
            z3::ast::Bool::and(&[&a_none, &b_none]),
        ])
    }

    fn maybe_str_leq(&self, a: &z3::ast::Datatype, b: &z3::ast::Datatype) -> z3::ast::Bool {
        let a_none = self.maybe_str.variants[0]
            .tester
            .apply(&[a])
            .as_bool()
            .unwrap();
        let a_some = self.maybe_str.variants[1]
            .tester
            .apply(&[a])
            .as_bool()
            .unwrap();
        let b_none = self.maybe_str.variants[0]
            .tester
            .apply(&[b])
            .as_bool()
            .unwrap();
        let b_some = self.maybe_str.variants[1]
            .tester
            .apply(&[b])
            .as_bool()
            .unwrap();

        let aval = self.maybe_str.variants[1].accessors[0]
            .apply(&[a])
            .as_string()
            .unwrap();
        let bval = self.maybe_str.variants[1].accessors[0]
            .apply(&[b])
            .as_string()
            .unwrap();

        z3::ast::Bool::or(&[
            z3::ast::Bool::and(&[&a_some, &b_some, &aval._eq(&bval)]),
            z3::ast::Bool::and(&[&a_some, &b_none]),
            z3::ast::Bool::and(&[&a_none, &b_none]),
        ])
    }
}

impl Enc {
    pub fn declare_rec_helpers(&mut self) {
        use z3::ast::{Ast, Bool, Datatype};
        use z3::Sort;

        // --- Declare the recursive functions
        self.tup_leq = Some(z3::RecFuncDecl::new(
            "TupLeq",
            &[&self.list_shape.sort, &self.list_shape.sort],
            &Sort::bool(),
        ));

        self.has_field = Some(z3::RecFuncDecl::new(
            "HasField",
            &[&self.list_field.sort, &self.str, &self.shape.sort],
            &Sort::bool(),
        ));

        self.obj_leq = Some(z3::RecFuncDecl::new(
            "ObjLeq",
            &[&self.list_field.sort, &self.list_field.sort],
            &Sort::bool(),
        ));

        // ============================================================
        // TupLeq(xs, ys): elementwise ≤ and same length
        // ============================================================
        let xs = Datatype::new_const("xs", &self.list_shape.sort);
        let ys = Datatype::new_const("ys", &self.list_shape.sort);

        let xs_nil = self.list_shape.variants[0]
            .tester
            .apply(&[&xs])
            .as_bool()
            .unwrap(); // LSNil?
        let ys_nil = self.list_shape.variants[0]
            .tester
            .apply(&[&ys])
            .as_bool()
            .unwrap();
        let xs_cons = self.list_shape.variants[1]
            .tester
            .apply(&[&xs])
            .as_bool()
            .unwrap(); // LSCons?
        let ys_cons = self.list_shape.variants[1]
            .tester
            .apply(&[&ys])
            .as_bool()
            .unwrap();

        let xh = self.list_shape.variants[1].accessors[0]
            .apply(&[&xs])
            .as_datatype()
            .unwrap(); // head
        let xt = self.list_shape.variants[1].accessors[1]
            .apply(&[&xs])
            .as_datatype()
            .unwrap(); // tail
        let yh = self.list_shape.variants[1].accessors[0]
            .apply(&[&ys])
            .as_datatype()
            .unwrap();
        let yt = self.list_shape.variants[1].accessors[1]
            .apply(&[&ys])
            .as_datatype()
            .unwrap();

        let heads = self.subtype.apply(&[&xh, &yh]).as_bool().unwrap();
        let tails = self
            .tup_leq
            .as_mut()
            .unwrap()
            .apply(&[&xt, &yt])
            .as_bool()
            .unwrap();

        let tup_body = Bool::or(&[
            Bool::and(&[&xs_nil, &ys_nil]),                   // [] ≤ []
            Bool::and(&[&xs_cons, &ys_cons, &heads, &tails]), // (x::xs) ≤ (y::ys)
        ]);
        self.tup_leq
            .as_mut()
            .unwrap()
            .add_def(&[&xs, &ys], &tup_body);

        // ============================================================
        // HasField(fs, name, want): list fs contains a field `name`
        // whose type ≤ `want`
        // ============================================================
        let fs = Datatype::new_const("fs", &self.list_field.sort);
        let name = Datatype::new_const("name", &self.str);
        let want = Datatype::new_const("want", &self.shape.sort);

        let fs_nil = self.list_field.variants[0]
            .tester
            .apply(&[&fs])
            .as_bool()
            .unwrap(); // LFNil?
        let fs_cons = self.list_field.variants[1]
            .tester
            .apply(&[&fs])
            .as_bool()
            .unwrap(); // LFCons?

        let f_head = self.list_field.variants[1].accessors[0]
            .apply(&[&fs])
            .as_datatype()
            .unwrap(); // Field
        let f_tail = self.list_field.variants[1].accessors[1]
            .apply(&[&fs])
            .as_datatype()
            .unwrap();

        let f_is_mk = self.field.variants[0]
            .tester
            .apply(&[&f_head])
            .as_bool()
            .unwrap(); // MkField?
        let f_name = self.field.variants[0].accessors[0].apply(&[&f_head]);
        let f_ty = self.field.variants[0].accessors[1]
            .apply(&[&f_head])
            .as_datatype()
            .unwrap();

        let name_eq = f_name._eq(&name);
        let ty_leq = self.subtype.apply(&[&f_ty, &want]).as_bool().unwrap();
        let tail_has = self
            .has_field
            .as_mut()
            .unwrap()
            .apply(&[&f_tail, &name, &want])
            .as_bool()
            .unwrap();

        // LFNil -> false
        let has_body = Bool::or(&[
            Bool::and(&[&fs_cons, &f_is_mk, &name_eq, &ty_leq]),
            Bool::and(&[&fs_cons, &tail_has]),
            Bool::and(&[&fs_nil, &Bool::from_bool(false)]), // explicit false for clarity
        ]);
        self.has_field
            .as_mut()
            .unwrap()
            .add_def(&[&fs, &name, &want], &has_body);

        // ============================================================
        // ObjLeq(lhs, rhs): every (k: tR) required by rhs
        // appears in lhs with a field type tL s.t. tL ≤ tR
        // ============================================================
        let lhs = Datatype::new_const("lhs", &self.list_field.sort);
        let rhs = Datatype::new_const("rhs", &self.list_field.sort);

        let rhs_nil = self.list_field.variants[0]
            .tester
            .apply(&[&rhs])
            .as_bool()
            .unwrap(); // LFNil?
        let rhs_cons = self.list_field.variants[1]
            .tester
            .apply(&[&rhs])
            .as_bool()
            .unwrap(); // LFCons?

        let rh = self.list_field.variants[1].accessors[0]
            .apply(&[&rhs])
            .as_datatype()
            .unwrap(); // Field
        let rt = self.list_field.variants[1].accessors[1]
            .apply(&[&rhs])
            .as_datatype()
            .unwrap();

        let r_is_mk = self.field.variants[0]
            .tester
            .apply(&[&rh])
            .as_bool()
            .unwrap(); // MkField?
        let r_name = self.field.variants[0].accessors[0].apply(&[&rh]);
        let r_ty = self.field.variants[0].accessors[1]
            .apply(&[&rh])
            .as_datatype()
            .unwrap();

        let need = self
            .has_field
            .as_mut()
            .unwrap()
            .apply(&[&lhs, &r_name, &r_ty])
            .as_bool()
            .unwrap();
        let rest = self
            .obj_leq
            .as_mut()
            .unwrap()
            .apply(&[&lhs, &rt])
            .as_bool()
            .unwrap();

        let obj_body = Bool::or(&[
            rhs_nil,                                         // {} ≤ {}
            Bool::and(&[&rhs_cons, &r_is_mk, &need, &rest]), // ensure each required field
        ]);
        self.obj_leq
            .as_mut()
            .unwrap()
            .add_def(&[&lhs, &rhs], &obj_body);
    }
}

impl Enc {
    pub fn unwrap_maybe_float(&self, m: &z3::ast::Datatype) -> (z3::ast::Real, z3::ast::Bool) {
        let is_some = self.maybe_float.variants[1]
            .tester
            .apply(&[m])
            .as_bool()
            .unwrap();
        let val = self.maybe_float.variants[1].accessors[0]
            .apply(&[m])
            .as_real()
            .unwrap();
        (val, is_some)
    }
}

pub fn lower_constraint(enc: &Enc, c: &Constraint) -> Bool {
    match c {
        Constraint::Rel { t1, rel, t2 } => {
            let a = to_z3_shape(enc, t1);
            let b = to_z3_shape(enc, t2);
            match rel {
                Relation::Equality(Equality::Equal) => a._eq(&b),
                Relation::Equality(Equality::NotEqual) => a._eq(&b).not(),
                Relation::Subtyping(Subtyping::Subtype) => {
                    enc.subtype.apply(&[&a, &b]).as_bool().unwrap()
                }
                Relation::Subtyping(Subtyping::Supertype) => {
                    enc.subtype.apply(&[&b, &a]).as_bool().unwrap()
                }
                // Comparisons only valid on numbers or lengths; enforce guards:
                Relation::Comparison(rel) => {
                    // Extract numeric refinements; add guards that both are Num(Some r)
                    todo!("")
                }
                Relation::Subtyping(Subtyping::Incompatible) => todo!(),
            }
        }
        Constraint::Conditional { c1, c2 } => {
            // Interpreted as implication: c1 ⇒ c2
            let p = lower_constraint(enc, c1);
            let q = lower_constraint(enc, c2);
            Bool::implies(&p, &q)
        }
        Constraint::Or(vs) => Bool::or(
            &vs.iter()
                .map(|c| lower_constraint(enc, c))
                .collect::<Vec<_>>(),
        ),
        Constraint::And(vs) => Bool::and(
            &vs.iter()
                .map(|c| lower_constraint(enc, c))
                .collect::<Vec<_>>(),
        ),
        Constraint::False => Bool::from_bool(false),
    }
}

pub fn to_z3_shape(enc: &Enc, s: &Shape) -> Datatype {
    let s = match s {
        Shape::Blob => enc.top(),
        Shape::Null => enc.null(),
        Shape::Bool(opt) => match opt {
            None => enc.bool_(),
            Some(b) => enc.bool(*b),
        },
        Shape::Number(f) => match f {
            None => enc.num_(),
            Some(n) => enc.num(*n),
        },
        Shape::String(s) => match s {
            None => enc.str_(),
            Some(s) => enc.str(s),
        },
        Shape::Array(elem, len_opt) => {
            let e = to_z3_shape(enc, elem);
            let ml = match len_opt {
                None => enc.maybe_int.variants[0]
                    .constructor
                    .apply(&[])
                    .as_datatype()
                    .unwrap(),
                Some(n) => {
                    let i = Int::from_i64(*n as i64);
                    enc.maybe_int.variants[1]
                        .constructor
                        .apply(&[&i])
                        .as_datatype()
                        .unwrap()
                }
            };
            enc.array(&e, &ml)
        }
        Shape::Tuple(elts) => {
            let mut list = enc.list_shape.variants[0]
                .constructor
                .apply(&[])
                .as_datatype()
                .unwrap(); // Nil
            for elt in elts.iter().rev() {
                let head = to_z3_shape(enc, elt);
                list = enc.list_shape.variants[1]
                    .constructor
                    .apply(&[&head, &list])
                    .as_datatype()
                    .unwrap();
            }
            enc.tuple(&list)
        }
        Shape::Object(kvs) => {
            let mut list = enc.list_field.variants[0]
                .constructor
                .apply(&[])
                .as_datatype()
                .unwrap(); // Nil

            for (k, v) in kvs.iter().rev() {
                let name = ast::String::new_const(k.as_str());
                let val = to_z3_shape(enc, v);
                let fld = enc.field.variants[0]
                    .constructor
                    .apply(&[&name, &val])
                    .as_datatype()
                    .unwrap();
                list = enc.list_field.variants[1]
                    .constructor
                    .apply(&[&fld, &list])
                    .as_datatype()
                    .unwrap();
            }
            enc.object(&list)
        }
        Shape::TVar(id) => enc.tvar(&Int::from(*id as u64)),
        Shape::Union(a, b) => {
            // Not an ADT case—represent Union via constraints when you generate them:
            // you’ll create a fresh t and assert a ≤ t ∧ b ≤ t (or t ≤ a ∨ t ≤ b depending on direction).
            panic!("Union should be compiled to constraints, not to a Shape node at solve time");
        }
        Shape::Cond(_, _) | Shape::Neg(_) | Shape::Mismatch(_, _) => {
            panic!("Compile Cond/Neg/Mismatch to constraints, not to a monolithic Shape term")
        }
    };
    assert_eq!(s.get_sort(), enc.shape.sort);
    s
}

#[cfg(test)]
mod tests {
    use z3::{SatResult, Solver};

    use super::*;

    #[test]
    fn test_tvar_to_z3() {
        let enc = Enc::new();
        let shape = Shape::TVar(1);
        let z3_shape = to_z3_shape(&enc, &shape);
        assert!(z3_shape == enc.tvar(&Int::from(1)));
    }

    #[test]
    fn test_array_to_z3() {
        let enc = Enc::new();
        let shape = Shape::Array(Box::new(Shape::TVar(1)), None);
        let z3_shape = to_z3_shape(&enc, &shape);
        assert!(
            z3_shape
                == enc.array(
                    &enc.tvar(&Int::from(1)),
                    &enc.maybe_int.variants[0]
                        .constructor
                        .apply(&[])
                        .as_datatype()
                        .unwrap()
                )
        );
    }

    #[test]
    fn test_object_to_z3() {
        let enc = Enc::new();
        let shape = Shape::Object(vec![
            (String::from("field1"), Shape::TVar(1)),
            (String::from("field2"), Shape::TVar(2)),
        ]);
        let obj = to_z3_shape(&enc, &shape);
        println!("obj: {:?}", obj);
    }

    #[test]
    fn test_bool_to_z3() {
        let enc = Enc::new();
        let shape = Shape::bool(true);
        let z3_shape = to_z3_shape(&enc, &shape);
        assert_eq!(z3_shape, enc.bool(true));
    }

    #[test]
    fn test_bool_none_to_z3() {
        let enc = Enc::new();
        let shape = Shape::bool_();
        let z3_shape = to_z3_shape(&enc, &shape);
        assert_eq!(z3_shape, enc.bool_());
    }

    #[test]
    fn test_num_to_z3() {
        let enc = Enc::new();
        let shape = Shape::number(3);
        let z3_shape = to_z3_shape(&enc, &shape);
        assert_eq!(z3_shape, enc.num(3.0));
    }

    #[test]
    fn test_string_to_z3() {
        let enc = Enc::new();
        let shape = Shape::string("hello");
        let z3_shape = to_z3_shape(&enc, &shape);
        assert_eq!(z3_shape, enc.str("hello"));
    }

    #[test]
    fn test_string_none_to_z3() {
        let enc = Enc::new();
        let shape = Shape::string_();
        let z3_shape = to_z3_shape(&enc, &shape);
        assert_eq!(z3_shape, enc.str_());
    }

    #[test]
    fn test_array_no_length_to_z3() {
        let enc = Enc::new();
        let shape = Shape::array(Shape::string("hello"), None);
        let z3_shape = to_z3_shape(&enc, &shape);
        assert_eq!(z3_shape, enc.array_(&enc.str("hello")));
    }

    #[test]
    fn test_array_with_length_to_z3() {
        let enc = Enc::new();
        let shape = Shape::array(Shape::string_(), Some(5));
        let z3_shape = to_z3_shape(&enc, &shape);
        println!("z3_shape: {:?}", z3_shape);
        assert_eq!(
            z3_shape,
            enc.array(
                &enc.str_(),
                &enc.maybe_int.variants[1]
                    .constructor
                    .apply(&[&Int::from(5 as i64)])
                    .as_datatype()
                    .unwrap()
            )
        );
    }

    #[test]
    fn test_simple_subtype() {
        let enc = Enc::new();
        let solver = Solver::new();
        enc.define_subtype();

        let bool_ = to_z3_shape(&enc, &Shape::bool_());
        let bool_true = to_z3_shape(&enc, &Shape::bool(true));

        // Check that the subtyping relation is satisfiable
        solver.assert(enc.subtype.apply(&[&bool_true, &bool_]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Sat);
        solver.reset();
        // Check that the subtyping relation HAS TO be correct
        solver.assert(!enc.subtype.apply(&[&bool_true, &bool_]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
        // Check that the reverse subtyping cannot be satisfied
        solver.assert(enc.subtype.apply(&[&bool_, &bool_true]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();

        let num_ = to_z3_shape(&enc, &Shape::number_());
        let num_5 = to_z3_shape(&enc, &Shape::number(5.0));
        // Check that the subtyping relation is satisfiable
        solver.assert(enc.subtype.apply(&[&num_5, &num_]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Sat);
        solver.reset();
        // Check that the subtyping relation HAS TO be correct
        solver.assert(!enc.subtype.apply(&[&num_5, &num_]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
        // Check that the reverse subtyping cannot be satisfied
        solver.assert(enc.subtype.apply(&[&num_, &num_5]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();

        let blob = to_z3_shape(&enc, &Shape::blob());
        // Check that anything is a subtype of blob
        solver.assert(!enc.subtype.apply(&[&bool_, &blob]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
        solver.assert(!enc.subtype.apply(&[&num_, &blob]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
        solver.assert(!enc.subtype.apply(&[&bool_true, &blob]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
        solver.assert(!enc.subtype.apply(&[&num_5, &blob]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
        // Check that blob is not a subtype of anything
        solver.assert(enc.subtype.apply(&[&blob, &bool_]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
        solver.assert(enc.subtype.apply(&[&blob, &num_]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
        solver.assert(enc.subtype.apply(&[&blob, &bool_true]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
        solver.assert(enc.subtype.apply(&[&blob, &num_5]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
    }

    #[test]
    fn test_array_subtyping() {
        let enc = Enc::new();
        let solver = Solver::new();
        enc.define_subtype();
        let empty_array = to_z3_shape(&enc, &Shape::array(Shape::blob(), None));
        let non_empty_array = to_z3_shape(&enc, &Shape::array(Shape::blob(), Some(5)));
        let longer_array = to_z3_shape(&enc, &Shape::array(Shape::blob(), Some(10)));
        let non_blob_empty_array = to_z3_shape(&enc, &Shape::array(Shape::number_(), None));
        let non_blob_non_empty_array = to_z3_shape(&enc, &Shape::array(Shape::number_(), Some(5)));
        // Check that the non-empty array is a subtype of the empty array
        solver.assert(
            enc.subtype
                .apply(&[&non_empty_array, &empty_array])
                .as_bool()
                .unwrap(),
        );
        assert!(solver.check() == SatResult::Sat);
        solver.reset();
        // Check that the empty array is not a subtype of the non-empty array
        solver.assert(
            !enc.subtype
                .apply(&[&non_empty_array, &empty_array])
                .as_bool()
                .unwrap(),
        );
        assert!(solver.check() == SatResult::Unsat);

        solver.reset();
        solver.assert(!enc.is_array(&longer_array));
        assert!(
            solver.check() == SatResult::Unsat,
            "{longer_array} is not an array"
        );

        solver.reset();
        solver.assert(!enc.is_array(&non_empty_array));
        assert!(
            solver.check() == SatResult::Unsat,
            "{non_empty_array} is not an array"
        );

        let longer_array_elem = longer_array.nth_child(0).unwrap().as_datatype().unwrap();
        let non_empty_array_elem = non_empty_array.nth_child(0).unwrap().as_datatype().unwrap();
        solver.reset();
        solver.assert(
            !enc.subtype
                .apply(&[&longer_array_elem, &non_empty_array_elem])
                .as_bool()
                .unwrap(),
        );
        assert!(
            solver.check() == SatResult::Unsat,
            "Element types of {longer_array_elem} and {non_empty_array_elem} are not in a subtyping relation"
        );

        let longer_array_len = longer_array.nth_child(1).unwrap().as_datatype().unwrap();
        let non_empty_array_len = non_empty_array.nth_child(1).unwrap().as_datatype().unwrap();
        solver.reset();
        solver.assert(!enc.maybe_int_leq_math(&longer_array_len, &non_empty_array_len));
        assert!(
            solver.check() == SatResult::Unsat,
            "Lengths of {longer_array_len} and {non_empty_array_len} are not in a subtyping relation"
        );
        solver.reset();
        // assert!(longer_array.nth_child(0).unwrap().is_blob());
        // Check that longer array is a subtype of shorter array
        solver.reset();
        solver.assert(
            !enc.subtype
                .apply(&[&longer_array, &non_empty_array])
                .as_bool()
                .unwrap(),
        );
        assert!(
            solver.check() == SatResult::Unsat,
            "{longer_array} is not a subtype of {non_empty_array}"
        );
        solver.reset();
        // Check that non-blob arrays are subtypes of blob arrays
        solver.assert(
            !enc.subtype
                .apply(&[&non_blob_empty_array, &empty_array])
                .as_bool()
                .unwrap(),
        );
        assert!(solver.check() == SatResult::Unsat);

        solver.reset();
        solver.assert(
            !enc.subtype
                .apply(&[&non_blob_non_empty_array, &non_empty_array])
                .as_bool()
                .unwrap(),
        );
        assert!(
            solver.check() == SatResult::Unsat,
            "Non-blob-non-empty-array {} is not a subtype of {}",
            non_blob_non_empty_array,
            non_empty_array
        );

        solver.reset();
        solver.assert(
            !enc.subtype
                .apply(&[&longer_array, &non_empty_array])
                .as_bool()
                .unwrap(),
        );
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
    }

    #[test]
    fn test_object_subtyping() {
        let solver = Solver::new();
        let enc = Enc::new();
        enc.define_subtype();

        let empty_object = to_z3_shape(&enc, &Shape::Object(vec![]));
        let singleton_empty_object =
            to_z3_shape(&enc, &Shape::Object(vec![("a".into(), Shape::number_())]));
        let two_field_object = to_z3_shape(
            &enc,
            &Shape::Object(vec![
                ("a".into(), Shape::number_()),
                ("b".into(), Shape::bool_()),
            ]),
        );
        let unrelated_object =
            to_z3_shape(&enc, &Shape::Object(vec![("c".into(), Shape::string_())]));

        // Every object is a subtype of the empty object
        solver.reset();
        solver.assert(
            !enc.subtype
                .apply(&[&singleton_empty_object, &empty_object])
                .as_bool()
                .unwrap(),
        );
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
        solver.assert(
            !enc.subtype
                .apply(&[&two_field_object, &empty_object])
                .as_bool()
                .unwrap(),
        );
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
        solver.assert(
            !enc.subtype
                .apply(&[&unrelated_object, &empty_object])
                .as_bool()
                .unwrap(),
        );
        assert!(solver.check() == SatResult::Unsat);

        // An object with keys a,b should be a subtype of an object with key a
        solver.reset();
        solver.assert(
            !enc.subtype
                .apply(&[&two_field_object, &singleton_empty_object])
                .as_bool()
                .unwrap(),
        );
        assert!(solver.check() == SatResult::Unsat);
        // An object with keys a,b should be not be a subtype of an object with key c
        solver.reset();
        solver.assert(
            enc.subtype
                .apply(&[&two_field_object, &unrelated_object])
                .as_bool()
                .unwrap(),
        );
        assert!(solver.check() == SatResult::Unsat);
    }
}
