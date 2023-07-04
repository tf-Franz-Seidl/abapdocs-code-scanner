interface lif_abapdocs_scan_tree.
  "! path of a method/type description in the scan tree
  types t_path     type standard table of string with default key.
  types t_abapdocs_source_lines type seo_section_source.

  methods get_abapdocs_for_path
    importing path            type lif_abapdocs_scan_tree=>t_path
    returning value(abapdocs) type lif_abapdocs_scan_tree=>t_abapdocs_source_lines.
  methods get_fields_with_annotation
    importing
              field_keyword        type string
              annotation_key       type string
    returning value(name_mappings) type zcl_acs_abapdocs_code_scanner=>name_mappings.

  methods get_errors
    returning value(error_messages) type zcl_acs_abapdocs_code_scanner=>t_error_messages.

endinterface.

class lcl_extractor definition.

  public section.
    class-methods split_into_annoation_key_value
      importing docs_line         type seo_section_source_line
      returning value(annotation) type zcl_acs_abapdocs_annotation=>t_key_value.
    class-methods extract_class_pool_name
      importing
        i_absolute_name          type abap_abstypename
      returning
        value(r_class_pool_name) type string
      raising
        cx_sy_matcher
        cx_sy_regex .

    class-methods get_path_for_typedescr
      importing absolute_name type abap_abstypename
      returning value(path)   type lif_abapdocs_scan_tree=>t_path.

  private section.
    class-data annotation_regex type ref to cl_abap_regex.

endclass.

class lcl_abapdocs_scan_tree definition deferred.

class lcl_scan_repo definition.
  public section.
    class-methods get_abapdocs_tree
      importing class_pool_name           type string
      returning value(abapdocs_scan_tree) type ref to lif_abapdocs_scan_tree.

  private section.
    types:
      begin of t_scan_cache,
        class_pool_name type string,
        abapdocs_tree   type ref to lif_abapdocs_scan_tree,
      end of t_scan_cache
      .

    class-data scan_cache type hashed table of t_scan_cache with unique key class_pool_name.

endclass.


class lcl_abapdocs_scan_tree definition.
  public section.
    interfaces lif_abapdocs_scan_tree.

    constants name_for_include type string value `.include`.

    types:
      begin of t_abapdocs_tree_node,
        parent_id               type int4,
        name                    type string,
        "! only if name = .include
        "! class or interface
        "! global or local
        include_type_class_name type string,
        "! only if name = .include
        include_type            type string,
        abapdocs                type seo_section_source,
        "! @enum: ["types",
        "!         "methods",
        "!         "class",
        "!         "interface"]
        keyword                 type string,
      end of t_abapdocs_tree_node,

      begin of t_abapdocs_tree_node_internal,
        id type int4.
        include type t_abapdocs_tree_node as node.
      types:
      end of t_abapdocs_tree_node_internal.
    .
    types t_abapdocs_tree type sorted table of lcl_abapdocs_scan_tree=>t_abapdocs_tree_node_internal with unique key id.

    methods insert
      importing
                parent_id      type int4
                keyword        type string
                name           type string
                abapdocs       type seo_section_source
      returning value(node_id) type int4.

    "! suffix is not respected at the moment
    methods insert_include
      importing
        parent_id    type int4
        include_type type string.

    methods resolve_includes.

  private section.
    methods get_next_id returning value(id) type int4.

    data abapdocs_tree type t_abapdocs_tree.

    data last_id type int4 value 0.

    data error_trace type zcl_acs_abapdocs_code_scanner=>t_error_messages.

endclass.


"! scans source code of a class/interface by using "read report" and "scan abap-source"<br/>
"! methods fill_tree is used to get all abapDocs from public fields/methods
class lcl_abapdocs_reposrc_scanner definition.
  public section.

    methods constructor.

    "! scans source of a class/interface and inserts all abapDocs of public fields/methods into scan_tree (input parameter)
    methods fill_tree
      importing class_pool_name type string
                scan_tree       type ref to lcl_abapdocs_scan_tree.

  protected section.

  private section.

    types: begin of t_scan,
             structures type sstruc_tab,
             tokens     type standard table of stokes with empty key,
             statements type sstmnt_tab,
             levels     type slevel_tab,
           end of t_scan.

    data regex_for_abapdocs_line type ref to cl_abap_regex.

    methods retrieve_abapdocs_for_stmt
      importing
        statement_id    type int4
        scan            type t_scan
      returning
        value(r_result) type seo_section_source.

    "! execute "scan abap-source"
    methods read_and_scan_code
      importing
                class_pool_name type string
      returning value(scan)     type t_scan.


    "! extract abapDocs from scan structure<br/>
    "! recursive calls for nested structures
    methods identify_comment_blocks
      importing
        value(parent_stucture_id)      type int4
        value(structure_id)            type int4
        value(abapdocs_tree_parent_id) type int4
        scan                           type t_scan
        scan_tree                      type ref to lcl_abapdocs_scan_tree
      .

    methods insert_statement
      importing
        value(parent_node_id) type int4
        stmt                  type sstmnt
        statement_id          type int4
        scan                  type t_scan
        scan_tree             type ref to lcl_abapdocs_scan_tree
      .

endclass.

class lcx_error definition inheriting from cx_no_check.
  public section.
    methods constructor
      importing
        message  type string
        previous like previous optional.
    data message type string read-only.
endclass.



**************************************************************************************************
**************************************************************************************************
* code below is just for the unit tests

types:
  "! f2-docs
  t_test_f2 type int4,
  "! doc of test_sub
  begin of t_abapdocs_test_sub,
    "! sub1-docs
    sub1 type string,
  end of t_abapdocs_test_sub,
  begin of t_abapdocs_test,
    "! f1-docs
    "! @jsonName: field1
    "! @enum: ["types",
    "!         "methods",
    "!         "class"]
    f1        type string,
    "! stru_1-docs
    begin of stru_1,
      "! s1_1-docs
      "! @huhu
      s1_1                 type string,
      "! TSAD2 - Academic Titles
      "! @enum = [ "Akad.Betr.Ök.", "Akad.Fin.Dienstl.", "Akad.Vkfm.", "Akad.Vkfr.", "Ass.Prof.Priv.Doz.Dr", "Avv.", "B. rer. nat.", "B.A.", "B.A.(Econ.)", "B.B.A.", "B.S.c.", "B.techn.", "BA psych.", "BA pth.",
      "! "BEd", "BSc", "BScN", "BStat", "BTh", "Bacc. rel. paed.", "Bakk (FH)", "Bakk.", "Bakk. (FH)", "Bakk. Soz.", "Bakk. iur.", "Bakk. theol.", "Bakk.Biol.", "Bakk.Komm.", "Bakk.Sport", "Bakk.art", "Bakk.phil",
      "! "Bakk.rer.nat.", "Bakk.rer.soc.oec.", "Bakk.soc.", "Bakk.techn.", "Beng", "DDDr.", "DDR.med.univ.", "DDipl.Ing.", "DDr.", "DDr.med", "DI", "DI.Dr.", "DI.Mag.Dr.", "Dipl.-Dolm.", "Dipl.-Inform.", "Dipl.-Ing.",
      "! "Dipl.-Kffr.", "Dipl.-Kfm.", "Dipl.-Oecothroph.", "Dipl.-Soz.ther.", "Dipl.-Vw.", "Dipl.Arch.", "Dipl.Bw.", "Dipl.Dolm.", "Dipl.Dolm.Dkfm.", "Dipl.Dolm.Dr.", "Dipl.Dolm.Mag.", "Dipl.Geol.", "Dipl.HLFL.Ing.",
      "! "Dipl.HTL.Ing.", "Dipl.Ing.", "Dipl.Ing.DDr.", "Dipl.Ing.Dkfm.", "Dipl.Ing.Dr.", "Dipl.Ing.FH.", "Dipl.Ing.Mag.", "Dipl.Ing.Mag.Dr.", "Dipl.Log.", "Dipl.Min.", "Dipl.PT", "Dipl.Phys.", "Dipl.Psych.",
      "! "Dipl.Päd." ]
      academical_title(20) type c,
    end of stru_1,
    "! sub_table-docs
    sub_table type standard table of t_abapdocs_test_sub with default key,

    "! @jsonName: field2
    f2        type  t_test_f2.
    include type zif_acs_abapdocs_example=>t_address_base.
  types:
  end of t_abapdocs_test.

"! class is just for the unit tests
class lcl_with_docs_v2 definition create public for testing.
  public section.
    methods constructor.
    "! meth1-docs
    methods meth1.
    "! meth2-docs
    methods meth2.
endclass.

"! class is just for the unit tests
class ltcl_class_with_abap_docs definition for testing.
  public section.
    "! abap_docs1
    methods meth1.
    "! only a literal
    types t_literal type string.
endclass.
