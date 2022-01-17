class ltcl_unittest definition deferred.
class ltcl_unittest_internal definition deferred.
class lcl_abapdocs_scan_tree definition deferred.
class ZCL_ACS_ABAPDOCS_CODE_SCANNER definition local friends lcl_abapdocs_scan_tree ltcl_unittest ltcl_unittest_internal.

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
    returning value(name_mappings) type /ui2/cl_json=>name_mappings.
endinterface.

class lcl_abapdocs_scan_tree definition.
  public section.
    interfaces lif_abapdocs_scan_tree.

    types: begin of t_abapdocs_tree_node,
             id        type int4,
             parent_id type int4,
             name      type string,
             abapdocs  type seo_section_source,
             "! @enum: ["types",
             "!         "methods",
             "!         "class"]
             keyword   type string,
           end of t_abapdocs_tree_node.
    types t_abapdocs_tree type sorted table of lcl_abapdocs_scan_tree=>t_abapdocs_tree_node with unique key id.

    "! @enum: ["types",
    "!         "methods",
    "!         "class"]
    methods insert
      importing
                parent_id      type int4
                name           type string
                abapdocs       type seo_section_source

                keyword        type string
      returning value(node_id) type int4.

  private section.
    methods get_next_id returning value(id) type int4.


    data abapdocs_tree type t_abapdocs_tree.

    data last_id type int4 value 0.
endclass.


"! scans source code of a class/interface by using "read report" and "scan abap-source"<br/>
"! methods fill_tree is used to get all abapDocs from public fields/methods
class lcl_abapdocs_reposrc_scanner definition.
  public section.
    type-pools scan.

    constants:
      "! Type of statement with possible values:
      "! also in SCAN_STRUC_STMNT_TYPE of type-pool scan
      begin of statement_type,
        class_definition     type flag value 'X',
        class_implementation type flag value 'Y',
        interface            type flag value 'Z',
        public_section       type flag value 'A',
        package_section      type flag value 'J',
        protected_section    type flag value 'B',
        private_section      type flag value 'G',

        "! Native SQL statement between EXEC SQL and ENDEXEC
        native_sql           type stmnt_type value 'E',
        "! INCLUDE prog
        include_prog         type stmnt_type value 'I',
        "! INCLUDE prog, prog does not exist, can occur only in connection with the addition WITH INCLUDES
        include_prog_ne      type stmnt_type value 'J',
        "! TYPE-POOLS pool
        type_pools           type stmnt_type value 'T',
        "! V (TYPE-POOLS pool, pool does not exist)
        type_pools_ne        type stmnt_type value 'V',
        "! R (call a macro from table TRMAC)
        macro_tab            type stmnt_type value 'R',
        "! D (call a macro internally defined with DEFINE)
        macro_def            type stmnt_type value 'D',
        "! M (macro definition between DEFINE and END-OF-DEFINITION)
        macro                type stmnt_type value 'M',
        "! C (COMPUTE statement, sometimes without COMPUTE as first token)
        compute              type stmnt_type value 'C',
        "! A (method call in short form)
        method_call          type stmnt_type value 'A',
        "! K (other ABAP key word)
        other                type stmnt_type value 'K',
        "! N (blank statement)
        blank                type stmnt_type value 'N',
        "! P (comment between statements)
        comment_between      type stmnt_type value 'P',
        "! S (comment within statements)
        comment_within       type stmnt_type value 'S',
        "! U (unknown, non-blank statement)
        unknown              type stmnt_type value 'U',
      end of statement_type,

      " Type of structure with possible values:
      begin of structure_type,
        "! P (beginning of the source code)
        beginning_of_source_code      type stru_type value 'P',
        "! R (subroutine)
        subroutine                    type stru_type value 'R',
        "! M (macro, EXEC SQL)
        macro                         type stru_type value 'M',
        "! I (loop)
        loop                          type stru_type value 'I',
        "! A (case distinction)
        case_distinction              type stru_type value 'A',
        "! C (condition in a case distinction)
        condition_in_case_distinction type stru_type value 'C',
        "! J (jump command)
        jump_command                  type stru_type value 'J',
        "! D (structured declaration)
        structured_declaration        type stru_type value 'D',
        "! E (event)
        event                         type stru_type value 'E',
        "! S (sequence of statements with simple structures)
        sequence_of_stats_with_struct type stru_type value 'S',
        "! L (class)
        class                         type stru_type value 'L',
      end of structure_type
      .

*              "! I (identifier)
*        identifier                   TYPE stru_type VALUE 'I',
*        "! S (string, hence a character literal)
*        string                       TYPE stru_type VALUE 'S',
*        "! L (list, enclosed in parentheses)
*        list                         TYPE stru_type VALUE 'L',
*        "! C (comment)
*        comment                      TYPE stru_type VALUE 'C',
*        "! B (beginning of a list)
*        beginning_of_a_list          TYPE stru_type VALUE 'B',
*        "! D (separator (divider) between list elements)
*        separator_between_list_elems TYPE stru_type VALUE 'D',
*        "! E (end of a list)
*        end_of_a_list                TYPE stru_type VALUE 'E',

    types t_possible_includes type standard table of progname with default key ##needed.

    "! scans source of a class/interface and inserts all abapDocs of public fields/methods into scan_tree (input parameter)
    class-methods fill_tree
      importing class_pool_name type string
                scan_tree       type ref to lcl_abapdocs_scan_tree.



    class-methods gen_possible_include_prognames
      importing class_pool_name          type string
      returning value(possible_includes) type lcl_abapdocs_reposrc_scanner=>t_possible_includes.

  protected section.

    types: begin of t_scan,
             structures type sstruc_tab,
             tokens     type standard table of stokes with empty key,
             statements type sstmnt_tab,
             levels     type slevel_tab,
           end of t_scan.



    class-methods retrieve_abapdocs_for_stmt
      importing
        statement_id    type i
        scan            type t_scan
      returning
        value(r_result) type seo_section_source.

    "! execute "scan abap-source"
    class-methods read_and_scan_code
      importing
                class_pool_name type string
      returning value(scan)     type t_scan.


    "! extract abapDocs from scan structure<br/>
    "! recursive calls for nested structures
    class-methods identify_comment_blocks
      importing
        value(parent_stucture_id)      type int4
        value(structure_id)            type int4
        value(abapdocs_tree_parent_id) type int4
        scan                           type t_scan
        scan_tree                      type ref to lcl_abapdocs_scan_tree
      .






  private section.



endclass.

class lcx_error definition inheriting from cx_no_check.
  public section.
    methods constructor
      importing
        message  type string
        previous like previous optional.
    data message type string read-only.
endclass.



*************************************************
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
      s1_1 type string,
    end of stru_1,
    "! sub_table-docs
    sub_table type standard table of t_abapdocs_test_sub with default key,

    "! @jsonName: field2
    f2        type  t_test_f2,
  end of t_abapdocs_test.

"! class is just for the unit tests
class lcl_with_docs_v2 definition create public.
  public section.
    methods constructor.
    "! meth1-docs
    methods meth1.
    "! meth2-docs
    methods meth2.
endclass.

"! class is just for the unit tests
class ltcl_class_with_abap_docs definition.
  public section.
    "! abap_docs1
    methods meth1.


    "! only a literal
    types t_literal type string.
endclass.
