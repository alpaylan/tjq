use z3::{
    ast::{self, Ast, Bool, Datatype, Float, Int},
    datatype_builder::create_datatypes,
    DatatypeAccessor, DatatypeBuilder, DatatypeSort, RecFuncDecl, Sort,
};

use crate::{
    experimental_type_inference::{Comparison, Constraint, Equality, Relation},
    Shape, Subtyping,
};

pub struct Enc {
    shape: DatatypeSort,
    list_shape: DatatypeSort,
    list_field: DatatypeSort,
    maybe_bool: DatatypeSort,
    maybe_float: DatatypeSort,
    maybe_int: DatatypeSort,
    maybe_str: DatatypeSort,
    field: DatatypeSort,
    str: Sort,
    // Subtype : Shape × Shape → Bool
    subtype: RecFuncDecl,
    kind_rank: RecFuncDecl,
    ordering: RecFuncDecl,
    // Scores
    pub score: RecFuncDecl,
    pub score_ls: RecFuncDecl,     // ListShape -> Int
    pub score_lf: RecFuncDecl,     // ListField -> Int
    pub score_ls_min: RecFuncDecl, // ListShape -> Int
    // Union
    pub all_sub: RecFuncDecl, // ListShape × Shape  -> Bool   (∀ a∈xs. a ≤ t)
    pub any_sub: RecFuncDecl, // Shape × ListShape  -> Bool   (∃ b∈ys. x ≤ b)

    // helpers (declare later in `declare_rec_helpers`)
    pub tup_leq: Option<RecFuncDecl>,
    pub has_field: Option<RecFuncDecl>,
    pub obj_leq: Option<RecFuncDecl>,
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
const IDX_UNION: usize = 10;

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
            .variant("TVar", vec![("id", DatatypeAccessor::Sort(Sort::int()))])
            .variant(
                "Union",
                vec![("options", DatatypeAccessor::Datatype("ListShape".into()))],
            );

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
        let kind_rank = z3::RecFuncDecl::new("JQKindRank", &[&shape.sort], &Sort::int());
        let ordering = z3::RecFuncDecl::new("JQLt", &[&shape.sort, &shape.sort], &Sort::bool());

        let score = z3::RecFuncDecl::new("Score", &[&shape.sort], &z3::Sort::int());
        let score_ls = z3::RecFuncDecl::new("ScoreLS", &[&list_shape.sort], &z3::Sort::int());
        let score_lf = z3::RecFuncDecl::new("ScoreLF", &[&list_field.sort], &z3::Sort::int());
        let score_ls_min = z3::RecFuncDecl::new("ScoreLSMin", &[&list_shape.sort], &Sort::int());

        let all_sub =
            z3::RecFuncDecl::new("AllSub", &[&list_shape.sort, &shape.sort], &Sort::bool());
        let any_sub =
            z3::RecFuncDecl::new("AnySub", &[&shape.sort, &list_shape.sort], &Sort::bool());

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

            // higher-order constraints
            subtype,
            kind_rank,
            ordering,
            // score
            score,
            score_ls,
            score_lf,
            score_ls_min,
            // union
            all_sub,
            any_sub,
            // recursive functions
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
        println!("num inner: {}", inner);
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

    // fn tvar(&self, id: &Int) -> Datatype {
    //     self.shape.variants[IDX_TVAR as usize]
    //         .constructor
    //         .apply(&[id])
    //         .as_datatype()
    //         .unwrap()
    // }
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
        // 3) Subtyping is reflexive
        let case_refl = x._eq(&y);
        // 4) Subtyping is transitive
        let z = Datatype::new_const("z", &self.shape.sort);
        let case_trans = Bool::and(&[
            &self.subtype.apply(&[&x, &z]).as_bool().unwrap(),
            &self.subtype.apply(&[&z, &y]).as_bool().unwrap(),
        ])
        .implies(&self.subtype.apply(&[&x, &y]).as_bool().unwrap());

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

        // ----- Union on the left: Union(xs) ≤ y  ⇔  AllSub(xs, y)
        let is_union_x = self.shape.variants[IDX_UNION]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let xs = self.shape.variants[IDX_UNION].accessors[0]
            .apply(&[&x])
            .as_datatype()
            .unwrap();
        let all_x_le_y = self.all_sub.apply(&[&xs, &y]).as_bool().unwrap();
        let case_union_left = Bool::and(&[&is_union_x, &all_x_le_y]);

        // ----- Union on the right: x ≤ Union(ys)  ⇔  AnySub(x, ys)
        let is_union_y = self.shape.variants[IDX_UNION]
            .tester
            .apply(&[&y])
            .as_bool()
            .unwrap();
        let ys = self.shape.variants[IDX_UNION].accessors[0]
            .apply(&[&y])
            .as_datatype()
            .unwrap();
        let any_x_le_y = self.any_sub.apply(&[&x, &ys]).as_bool().unwrap();
        let case_union_right = Bool::and(&[&is_union_y, &any_x_le_y]);

        // Or all cases together
        let body = Bool::or(&[
            &case_bot, &case_top, &case_null, &case_bool, &case_num, &case_str, &case_arr,
            &case_tup, &case_obj,
            &case_refl,
            // &case_trans,
            // &case_union_left,
            // &case_union_right,
        ]);
        println!("Subtype body: {}", body);

        // Add recursive definition: Subtype(x,y) := body
        self.subtype.add_def(&[&x, &y], &body);
    }

    pub fn define_ordering(&self) {
        // -------------------------
        // JQKindRank(x) : Int
        // -------------------------
        let x = Datatype::new_const("x", &self.shape.sort);

        let is_null = self.shape.variants[IDX_NULL]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_bool = self.shape.variants[IDX_BOOL]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_num = self.shape.variants[IDX_NUM]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_str = self.shape.variants[IDX_STR]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_arr = self.shape.variants[IDX_ARR]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_obj = self.shape.variants[IDX_OBJ]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();

        // rank(null)=0, bool=1, num=2, str=3, arr=4, obj=5
        let body_rank = Bool::ite(
            &is_null,
            &Int::from_i64(0),
            &Bool::ite(
                &is_bool,
                &Int::from_i64(1),
                &Bool::ite(
                    &is_num,
                    &Int::from_i64(2),
                    &Bool::ite(
                        &is_str,
                        &Int::from_i64(3),
                        &Bool::ite(&is_arr, &Int::from_i64(4), &Int::from_i64(5)),
                    ),
                ),
            ),
        );

        self.kind_rank.add_def(&[&x], &body_rank);

        // -------------------------
        // JQLt(x,y) : Bool
        // -------------------------
        let y = Datatype::new_const("y", &self.shape.sort);

        let kx = self.kind_rank.apply(&[&x]).as_int().unwrap();
        let ky = self.kind_rank.apply(&[&y]).as_int().unwrap();

        let kinds_lt = kx.lt(&ky);
        let kinds_eq = kx._eq(&ky);

        // Within-kind: Booleans (false < true)
        // Bool(opt: MaybeBool)
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
            .unwrap();
        let bopt_y = self.shape.variants[IDX_BOOL].accessors[0]
            .apply(&[&y])
            .as_datatype()
            .unwrap();

        // MaybeBool = MBNone | MBSome(val: Bool)
        let x_b_some = self.maybe_bool.variants[1]
            .tester
            .apply(&[&bopt_x])
            .as_bool()
            .unwrap();
        let y_b_some = self.maybe_bool.variants[1]
            .tester
            .apply(&[&bopt_y])
            .as_bool()
            .unwrap();
        let x_b_val = self.maybe_bool.variants[1].accessors[0]
            .apply(&[&bopt_x])
            .as_bool()
            .unwrap();
        let y_b_val = self.maybe_bool.variants[1].accessors[0]
            .apply(&[&bopt_y])
            .as_bool()
            .unwrap();

        // false < true  is  (!x) & y
        let bool_lt = Bool::and(&[&x_b_some, &y_b_some, &x_b_val.not(), &y_b_val]);

        // Within-kind: Numbers (Real)
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

        let (x_n_val, x_n_some) = self.unwrap_maybe_float(&nopt_x); // -> (Real, Bool)
        let (y_n_val, y_n_some) = self.unwrap_maybe_float(&nopt_y);

        let num_lt = Bool::and(&[&x_n_some, &y_n_some, &x_n_val.lt(&y_n_val)]);

        // Combine within-kind comparators we support
        let within_kind_lt = Bool::or(&[
            Bool::and(&[&is_bool_x, &is_bool_y, &bool_lt]),
            Bool::and(&[&is_num_x, &is_num_y, &num_lt]),
            // Strings/arrays/objects have no tie-breaker here (fine for is* tests).
        ]);

        let body_lt = Bool::or(&[&kinds_lt, &Bool::and(&[&kinds_eq, &within_kind_lt])]);

        self.ordering.add_def(&[&x, &y], &body_lt);
    }

    pub fn define_score_min(&self) {
        let xs = Datatype::new_const("xs", &self.list_shape.sort);
        let xs_nil = self.list_shape.variants[0]
            .tester
            .apply(&[&xs])
            .as_bool()
            .unwrap();
        let xs_cons = self.list_shape.variants[1]
            .tester
            .apply(&[&xs])
            .as_bool()
            .unwrap();
        let xh = self.list_shape.variants[1].accessors[0]
            .apply(&[&xs])
            .as_datatype()
            .unwrap();
        let xt = self.list_shape.variants[1].accessors[1]
            .apply(&[&xs])
            .as_datatype()
            .unwrap();

        let inf = Int::from_i64(1_000_000);
        let s_head = self.score.apply(&[&xh]).as_int().unwrap();
        let s_tail = self.score_ls_min.apply(&[&xt]).as_int().unwrap();
        let min_ht = Bool::ite(&s_head.le(&s_tail), &s_head, &s_tail);

        let body = Bool::ite(&xs_nil, &inf, &min_ht);
        self.score_ls_min.add_def(&[&xs], &body);
    }

    pub fn define_score(&self) {
        let x = Datatype::new_const("sx", &self.shape.sort);

        // testers
        let is_top = self.shape.variants[0]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_bot = self.shape.variants[1]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_null = self.shape.variants[2]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_bool = self.shape.variants[3]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_num = self.shape.variants[4]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_str = self.shape.variants[5]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_arr = self.shape.variants[6]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_tup = self.shape.variants[7]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_obj = self.shape.variants[8]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_tvar = self.shape.variants[9]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        let is_union = self.shape.variants[10]
            .tester
            .apply(&[&x])
            .as_bool()
            .unwrap();
        // fields
        let bopt = self.shape.variants[3].accessors[0]
            .apply(&[&x])
            .as_datatype()
            .unwrap(); // MaybeBool
        let nopt = self.shape.variants[4].accessors[0]
            .apply(&[&x])
            .as_datatype()
            .unwrap(); // MaybeReal
        let sopt = self.shape.variants[5].accessors[0]
            .apply(&[&x])
            .as_datatype()
            .unwrap(); // MaybeStr
        let elem = self.shape.variants[6].accessors[0]
            .apply(&[&x])
            .as_datatype()
            .unwrap(); // Shape
        let alen = self.shape.variants[6].accessors[1]
            .apply(&[&x])
            .as_datatype()
            .unwrap(); // MaybeInt
        let elts = self.shape.variants[7].accessors[0]
            .apply(&[&x])
            .as_datatype()
            .unwrap(); // ListShape
        let flds = self.shape.variants[8].accessors[0]
            .apply(&[&x])
            .as_datatype()
            .unwrap(); // ListField

        // maybe penalties: None=0, Some(_)=1
        let mb_none = self.maybe_bool.variants[0]
            .tester
            .apply(&[&bopt])
            .as_bool()
            .unwrap();
        let mr_none = self.maybe_float.variants[0]
            .tester
            .apply(&[&nopt])
            .as_bool()
            .unwrap();
        let ms_none = self.maybe_str.variants[0]
            .tester
            .apply(&[&sopt])
            .as_bool()
            .unwrap();
        let mi_none = self.maybe_int.variants[0]
            .tester
            .apply(&[&alen])
            .as_bool()
            .unwrap();

        let p_bool = Bool::ite(&mb_none, &Int::from_i64(0), &Int::from_i64(1));
        let p_num = Bool::ite(&mr_none, &Int::from_i64(0), &Int::from_i64(1));
        let p_str = Bool::ite(&ms_none, &Int::from_i64(0), &Int::from_i64(1));
        let p_len = Bool::ite(&mi_none, &Int::from_i64(0), &Int::from_i64(1));

        // recursive scores
        let s_elem = self.score.apply(&[&elem]).as_int().unwrap();
        let s_ls = self.score_ls.apply(&[&elts]).as_int().unwrap();
        let s_lf = self.score_lf.apply(&[&flds]).as_int().unwrap();

        // Union scores
        let alts = self.shape.variants[IDX_UNION].accessors[0]
            .apply(&[&x])
            .as_datatype()
            .unwrap();
        let min_alt = self.score_ls_min.apply(&[&alts]).as_int().unwrap();

        // prefer union slightly over its best member: max(0, min_alt - 1)
        let union_score = Bool::ite(
            &min_alt.le(&Int::from_i64(0)),
            &Int::from_i64(0),
            &(min_alt - Int::from_i64(1)),
        );

        // weights (tune if needed)
        let body = is_top.ite(
            &Int::from_i64(0),
            &is_bot.ite(
                &Int::from_i64(10_000),
                &is_null.ite(
                    &(Int::from_i64(10)),
                    &is_bool.ite(
                        &(Int::from_i64(10) + p_bool),
                        &is_num.ite(
                            &(Int::from_i64(10) + p_num),
                            &is_str.ite(
                                &(Int::from_i64(10) + p_str),
                                &is_arr.ite(
                                    &(Int::from_i64(20) + s_elem + p_len),
                                    &is_tup.ite(
                                        &(Int::from_i64(30) + s_ls),
                                        &is_obj.ite(
                                            &(Int::from_i64(40) + s_lf),
                                            &is_tvar.ite(
                                                &Int::from_i64(5),
                                                &is_union.ite(&union_score, &Int::from_i64(9)),
                                            ),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        );

        self.score.add_def(&[&x], &body);

        // list scores (sum)
        let xs = Datatype::new_const("xs", &self.list_shape.sort);
        let xs_nil = self.list_shape.variants[0]
            .tester
            .apply(&[&xs])
            .as_bool()
            .unwrap();
        let xs_cons = self.list_shape.variants[1]
            .tester
            .apply(&[&xs])
            .as_bool()
            .unwrap();
        let xh = self.list_shape.variants[1].accessors[0]
            .apply(&[&xs])
            .as_datatype()
            .unwrap();
        let xt = self.list_shape.variants[1].accessors[1]
            .apply(&[&xs])
            .as_datatype()
            .unwrap();
        let s_head = self.score.apply(&[&xh]).as_int().unwrap();
        let s_tail = self.score_ls.apply(&[&xt]).as_int().unwrap();
        let ls_body = z3::ast::Bool::ite(&xs_nil, &Int::from_i64(0), &(s_head + s_tail));
        self.score_ls.add_def(&[&xs], &ls_body);

        let fs = Datatype::new_const("fs", &self.list_field.sort);
        let fs_nil = self.list_field.variants[0]
            .tester
            .apply(&[&fs])
            .as_bool()
            .unwrap();
        let fs_cons = self.list_field.variants[1]
            .tester
            .apply(&[&fs])
            .as_bool()
            .unwrap();
        let fh = self.list_field.variants[1].accessors[0]
            .apply(&[&fs])
            .as_datatype()
            .unwrap();
        let ft = self.list_field.variants[1].accessors[1]
            .apply(&[&fs])
            .as_datatype()
            .unwrap();
        let f_is_mk = self.field.variants[0]
            .tester
            .apply(&[&fh])
            .as_bool()
            .unwrap();
        let f_ty = self.field.variants[0].accessors[1]
            .apply(&[&fh])
            .as_datatype()
            .unwrap();
        let s_f_ty = self.score.apply(&[&f_ty]).as_int().unwrap();
        // field cost = 2 + score(ty)
        let field_cost = Int::from_i64(2) + s_f_ty;
        let lf_body = Bool::ite(
            &fs_nil,
            &Int::from_i64(0),
            &Bool::ite(
                &Bool::and(&[&fs_cons, &f_is_mk]),
                &(field_cost + self.score_lf.apply(&[&ft]).as_int().unwrap()),
                &Int::from_i64(0),
            ), // unreachable branch safeguard
        );
        self.score_lf.add_def(&[&fs], &lf_body);
    }

    pub fn declare_union_helpers(&self) {
        // -------- AllSub(xs, t) --------
        let xs = Datatype::new_const("xs", &self.list_shape.sort);
        let t = Datatype::new_const("t", &self.shape.sort);

        let xs_nil = self.list_shape.variants[0]
            .tester
            .apply(&[&xs])
            .as_bool()
            .unwrap();
        let xs_cons = self.list_shape.variants[1]
            .tester
            .apply(&[&xs])
            .as_bool()
            .unwrap();
        let xh = self.list_shape.variants[1].accessors[0]
            .apply(&[&xs])
            .as_datatype()
            .unwrap();
        let xt = self.list_shape.variants[1].accessors[1]
            .apply(&[&xs])
            .as_datatype()
            .unwrap();

        let head_ok = self.subtype.apply(&[&xh, &t]).as_bool().unwrap();
        let tail_ok = self.all_sub.apply(&[&xt, &t]).as_bool().unwrap();

        let all_body = Bool::or(&[
            Bool::and(&[&xs_nil, &Bool::from_bool(true)]),
            Bool::and(&[&xs_cons, &head_ok, &tail_ok]),
        ]);
        self.all_sub.add_def(&[&xs, &t], &all_body);

        // -------- AnySub(x, ys) --------
        let x = Datatype::new_const("x", &self.shape.sort);
        let ys = Datatype::new_const("ys", &self.list_shape.sort);

        let ys_nil = self.list_shape.variants[0]
            .tester
            .apply(&[&ys])
            .as_bool()
            .unwrap();
        let ys_cons = self.list_shape.variants[1]
            .tester
            .apply(&[&ys])
            .as_bool()
            .unwrap();
        let yh = self.list_shape.variants[1].accessors[0]
            .apply(&[&ys])
            .as_datatype()
            .unwrap();
        let yt = self.list_shape.variants[1].accessors[1]
            .apply(&[&ys])
            .as_datatype()
            .unwrap();

        let head_has = self.subtype.apply(&[&x, &yh]).as_bool().unwrap();
        let tail_has = self.any_sub.apply(&[&x, &yt]).as_bool().unwrap();

        let any_body = Bool::or(&[
            Bool::and(&[&ys_nil, &Bool::from_bool(false)]),
            Bool::and(&[&ys_cons, &head_has]),
            Bool::and(&[&ys_cons, &tail_has]),
        ]);
        self.any_sub.add_def(&[&x, &ys], &any_body);
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
    pub fn unwrap_maybe_float(&self, m: &z3::ast::Datatype) -> (z3::ast::Float, z3::ast::Bool) {
        let is_some = self.maybe_float.variants[1]
            .tester
            .apply(&[m])
            .as_bool()
            .unwrap();
        let val = self.maybe_float.variants[1].accessors[0]
            .apply(&[m])
            .as_float()
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
                Relation::Comparison(rel) => match rel {
                    Comparison::GreaterThan => enc.ordering.apply(&[&b, &a]).as_bool().unwrap(),
                    Comparison::LessThan => enc.ordering.apply(&[&a, &b]).as_bool().unwrap(),
                },
                Relation::Subtyping(Subtyping::Incompatible) => Bool::and(&[
                    enc.subtype.apply(&[&a, &b]).as_bool().unwrap().not(),
                    enc.subtype.apply(&[&b, &a]).as_bool().unwrap().not(),
                ]),
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
        Shape::TVar(id) => Datatype::new_const(format!("t_{id}"), &enc.shape.sort),
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
    use z3::{Optimize, SatResult, Solver};

    use super::*;

    #[test]
    #[ignore = "not sure how to model tvars yet"]
    fn test_tvar_to_z3() {
        let enc = Enc::new();
        let shape = Shape::TVar(1);
        let z3_shape = to_z3_shape(&enc, &shape);
        // assert!(z3_shape == enc.tvar(&Int::from(1)));
    }

    #[test]
    fn test_array_to_z3() {
        let enc = Enc::new();
        let shape = Shape::Array(Box::new(Shape::Null), None);
        let z3_shape = to_z3_shape(&enc, &shape);
        assert!(
            z3_shape
                == enc.array(
                    &enc.null(),
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

    #[test]
    fn test_simple_ordering() {
        let solver = Solver::new();
        let enc = Enc::new();
        enc.define_ordering();

        let false_ = to_z3_shape(&enc, &Shape::bool(false));
        let true_ = to_z3_shape(&enc, &Shape::bool(true));
        let bool_ = to_z3_shape(&enc, &Shape::bool_());

        let five_ = to_z3_shape(&enc, &Shape::number(5));
        let ten_ = to_z3_shape(&enc, &Shape::number(10));
        let num_ = to_z3_shape(&enc, &Shape::number_());

        let hello_ = to_z3_shape(&enc, &Shape::string("hello"));
        let str_ = to_z3_shape(&enc, &Shape::string_());

        solver.reset();
        solver.assert(!enc.ordering.apply(&[&false_, &true_]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
        solver.assert(!enc.ordering.apply(&[&five_, &ten_]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
        solver.assert(!enc.ordering.apply(&[&five_, &str_]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
        solver.assert(!enc.ordering.apply(&[&true_, &hello_]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
        solver.assert(!enc.ordering.apply(&[&false_, &str_]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);

        solver.reset();
        solver.assert(!enc.ordering.apply(&[&bool_, &num_]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
        solver.assert(!enc.ordering.apply(&[&bool_, &five_]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
        solver.reset();
        solver.assert(!enc.ordering.apply(&[&num_, &hello_]).as_bool().unwrap());
        assert!(solver.check() == SatResult::Unsat);
    }

    #[test]
    fn test_collection_ordering() {}

    #[test]
    fn test_subtyping_reflexive() {
        let solver = Solver::new();
        let enc = Enc::new();
        enc.define_subtype();

        // define 3 tvars
        let t0 = to_z3_shape(&enc, &Shape::TVar(0));
        let t1 = to_z3_shape(&enc, &Shape::TVar(1));
        // assert t0 ≤ t1 and t1 ≤ t2
        solver.assert(t0._eq(&t1));
        solver.assert(!enc.subtype.apply(&[&t0, &t1]).as_bool().unwrap());
        assert_eq!(
            solver.check(),
            SatResult::Unsat,
            "Subtyping reflexivity failed"
        );
        solver.reset();
        solver.assert(t0._eq(&t1));
        println!("t0: {:?}, t1: {:?}", t0, t1);
        println!("solver: {:?}", solver.get_assertions());
        solver.assert(enc.subtype.apply(&[&t0, &t1]).as_bool().unwrap());
        assert_eq!(
            solver.check(),
            SatResult::Sat,
            "Subtyping reflexivity failed"
        );
    }

    #[test]
    #[ignore = "Adding `case_trans` kills the performance, perhaps because it turns the problem into a search problem"]
    fn test_subtyping_transitive() {
        let solver = Solver::new();
        let enc = Enc::new();
        enc.define_subtype();

        // define 3 tvars
        let t0 = to_z3_shape(&enc, &Shape::TVar(0));
        let t1 = to_z3_shape(&enc, &Shape::TVar(1));
        let t2 = to_z3_shape(&enc, &Shape::TVar(2));
        // assert t0 ≤ t1 and t1 ≤ t2
        let t0_leq_t1 = enc.subtype.apply(&[&t0, &t1]).as_bool().unwrap();
        let t1_leq_t2 = enc.subtype.apply(&[&t1, &t2]).as_bool().unwrap();
        solver.assert(&t0_leq_t1);
        solver.assert(&t1_leq_t2);
        // Check if subtyping transitivity holds
        let t0_leq_t2 = enc.subtype.apply(&[&t0, &t2]).as_bool().unwrap();
        solver.assert(&t0_leq_t2);
        assert_eq!(
            solver.check(),
            SatResult::Sat,
            "Subtyping transitivity failed"
        );
    }

    #[test]
    fn test_implication() {
        // let solver = Solver::new();
        let opt = Optimize::new();
        let enc = Enc::new();
        enc.define_subtype();
        enc.define_ordering();
        enc.define_score();

        let t1 = to_z3_shape(&enc, &Shape::TVar(1));
        let s = enc.score.apply(&[&t1]).as_int().unwrap(); // Int
        opt.minimize(&s);
        let t2 = to_z3_shape(&enc, &Shape::TVar(2));
        let s2 = enc.score.apply(&[&t2]).as_int().unwrap(); // Int
        opt.minimize(&s2);
        let t3 = to_z3_shape(&enc, &Shape::TVar(3));
        let s3 = enc.score.apply(&[&t3]).as_int().unwrap(); // Int
        opt.minimize(&s3);
        let t4 = to_z3_shape(&enc, &Shape::TVar(4));
        let s4 = enc.score.apply(&[&t4]).as_int().unwrap(); // Int
        opt.minimize(&s4);

        // <T1> == <T3>
        opt.assert(&t1._eq(&t3));
        // <T4> == 1
        opt.assert(&t4._eq(to_z3_shape(&enc, &Shape::number(1.0))));
        // T3 is not a subtype of bool
        opt.assert(
            &!enc
                .subtype
                .apply(&[&t3, &to_z3_shape(&enc, &Shape::bool_())])
                .as_bool()
                .unwrap(),
        );
        // t4 is not a subtype of bool
        opt.assert(
            &!enc
                .subtype
                .apply(&[&t4, &to_z3_shape(&enc, &Shape::bool_())])
                .as_bool()
                .unwrap(),
        );
        // (<T3> == <T4> | <T3> == <null> | <T4> == <null>)
        opt.assert(&z3::ast::Bool::or(&[
            t3._eq(&t4),
            t3._eq(&to_z3_shape(&enc, &Shape::null())),
            t4._eq(&to_z3_shape(&enc, &Shape::null())),
        ]));
        // <T3> == <null> ==> <T2> <: <T4>
        opt.assert(&z3::ast::Bool::implies(
            &t3._eq(&to_z3_shape(&enc, &Shape::null())),
            enc.subtype.apply(&[&t2, &t4]).as_bool().unwrap(),
        ));
        // <T4> == <null> ==> <T2> <: <T3>
        opt.assert(&z3::ast::Bool::implies(
            &t4._eq(&to_z3_shape(&enc, &Shape::null())),
            enc.subtype.apply(&[&t2, &t3]).as_bool().unwrap(),
        ));
        assert_eq!(opt.check(&[]), SatResult::Sat);
        // get the model
        let model = opt.get_model().unwrap();
        // get the values of the type variables
        let t1_val = model.get_const_interp(&t1);
        let t2_val = model.get_const_interp(&t2);
        let t3_val = model.get_const_interp(&t3);
        let t4_val = model.get_const_interp(&t4);
        println!(
            "t1: {:?}, t2: {:?}, t3: {:?}, t4: {:?}",
            t1_val, t2_val, t3_val, t4_val
        );
    }
}
