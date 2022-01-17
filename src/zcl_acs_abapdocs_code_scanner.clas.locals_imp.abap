class lcl_abapdocs_scan_tree implementation.

  method insert.
    insert value #(
            id = get_next_id( )
            parent_id = parent_id
            name = name
            abapdocs = abapdocs
            keyword = keyword
     ) into table abapdocs_tree reference into data(new_node).
    node_id = new_node->id.
  endmethod.

  method get_next_id.
    last_id = last_id + 1.
    id = last_id.
  endmethod.

  method lif_abapdocs_scan_tree~get_abapdocs_for_path.
    data(id) = 0.
    try.
        loop at path into data(path_node).
          id = abapdocs_tree[ name = |{ path_node case = lower }| parent_id = id ]-id.
        endloop.
        abapdocs = abapdocs_tree[ id = id ]-abapdocs.
      catch cx_sy_itab_line_not_found.
        " nothing found for the passed path
    endtry.
  endmethod.


  method lif_abapdocs_scan_tree~get_fields_with_annotation.
    loop at abapdocs_tree assigning field-symbol(<field>) where keyword = field_keyword.
      loop at <field>-abapdocs into data(docs_line).
        data(annotation) = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>split_into_annoation_key_value( docs_line ).
        if annotation-key = annotation_key.
          insert value #( abap = |{ <field>-name case = upper }| json = annotation-value ) into table name_mappings.
        endif.
      endloop.
    endloop.
  endmethod.

endclass.

class lcl_abapdocs_reposrc_scanner implementation.

  method identify_comment_blocks.
    " preconditions
    if scan-structures is initial or
       scan-statements is initial or
       scan-tokens is initial.
      return.
    endif.

    field-symbols:
      <struc> type sstruc,
      <stmnt> type sstmnt.

    data(scan_structrue) = scan-structures[ structure_id ].
    data(scan_statement_1) = scan-statements[ scan_structrue-stmnt_from ].
    data(scan_statement_1_keyword) = scan-tokens[ scan_statement_1-from ]-str.

    case scan_structrue-stmnt_type.

      when scan_struc_stmnt_type-interface or
           scan_struc_stmnt_type-class_definition.

        data(class_node_id) = scan_tree->insert(
          parent_id = abapdocs_tree_parent_id
          name = |{ scan-tokens[ ( scan_statement_1-from + 1 ) ]-str case = lower }|
          abapdocs = retrieve_abapdocs_for_stmt( statement_id = scan_structrue-stmnt_from scan = scan )
          keyword = scan_statement_1_keyword
         ).

        loop at scan-structures assigning <struc>
            from scan_structrue-struc_from
            to   scan_structrue-struc_to.
          identify_comment_blocks(
              parent_stucture_id        = structure_id
              structure_id              = sy-tabix
              abapdocs_tree_parent_id   = class_node_id
              scan                      = scan
              scan_tree                 = scan_tree
          ).
        endloop.

      when scan_struc_stmnt_type-public_section.

        if   scan_structrue-struc_from <= scan_structrue-struc_to.
        " has sub structures

          loop at scan-structures assigning <struc>
            from scan_structrue-struc_from
            to   scan_structrue-struc_to.
            identify_comment_blocks(
                parent_stucture_id      = structure_id
                structure_id            = sy-tabix
                abapdocs_tree_parent_id = abapdocs_tree_parent_id
                scan                    = scan
                scan_tree               = scan_tree
            ).
          endloop.

        else.

          loop at scan-statements assigning <stmnt>
             from scan_structrue-stmnt_from
             to   scan_structrue-stmnt_to
             where type = scan_stmnt_type-standard.

            scan_tree->insert(
              parent_id = abapdocs_tree_parent_id
              name      = |{ scan-tokens[ ( <stmnt>-from + 1 ) ]-str case = lower }|
              abapdocs  = retrieve_abapdocs_for_stmt( statement_id = sy-tabix scan = scan )
              keyword   = |{ scan-tokens[ ( <stmnt>-from ) ]-str }|
             ).
          endloop.

        endif.

      when scan_struc_stmnt_type-sequence.
        " can be a list of method or type declaration or maybe something else

        if scan_statement_1_keyword <> 'INTERFACES'.
          loop at scan-statements assigning <stmnt>
              from scan_structrue-stmnt_from
              to   scan_structrue-stmnt_to
              where type = scan_stmnt_type-standard.

            scan_tree->insert(
              parent_id = abapdocs_tree_parent_id
              name = |{ scan-tokens[ ( <stmnt>-from + 1 ) ]-str case = lower }|
              abapdocs = retrieve_abapdocs_for_stmt( statement_id = sy-tabix scan = scan )
              keyword = |{ scan-tokens[ ( <stmnt>-from ) ]-str }|
             ).
          endloop.

        endif.

      when scan_struc_stmnt_type-method.

        scan_tree->insert(
          parent_id = abapdocs_tree_parent_id
          name = |{ scan-tokens[ ( scan_statement_1-from + 1 ) ]-str case = lower }|
          abapdocs = retrieve_abapdocs_for_stmt( statement_id = scan_structrue-stmnt_from scan = scan )
          keyword = scan_statement_1_keyword
         ).

      when scan_struc_stmnt_type-types.

        data(type_name) = |{ scan-tokens[ ( scan_statement_1-from + 1 ) ]-str case = lower }|.
        if type_name = 'begin'. "begin of
          type_name  = |{ scan-tokens[ ( scan_statement_1-from + 3 ) ]-str case = lower }|.
        endif.

        data(type_node_id) = scan_tree->insert(
          parent_id = abapdocs_tree_parent_id
          name = type_name
          abapdocs = retrieve_abapdocs_for_stmt( statement_id = scan_structrue-stmnt_from scan = scan )
          keyword = scan_statement_1_keyword
         ).

        if scan_structrue-struc_from <= scan_structrue-struc_to.
          loop at scan-structures assigning <struc>
              from scan_structrue-struc_from
              to   scan_structrue-struc_to.
            identify_comment_blocks(
                parent_stucture_id = structure_id
                structure_id       = sy-tabix
                abapdocs_tree_parent_id = type_node_id
                scan = scan
                scan_tree = scan_tree
            ).
          endloop.
        else.
          loop at scan-statements assigning <stmnt>
              from scan_structrue-stmnt_from + 1
              to   scan_structrue-stmnt_to
              where type = scan_stmnt_type-standard.

            scan_tree->insert(
              parent_id = type_node_id
              name = |{ scan-tokens[ ( <stmnt>-from + 1 ) ]-str case = lower }|
              abapdocs = retrieve_abapdocs_for_stmt( statement_id = sy-tabix scan = scan )
              keyword = |{ scan-tokens[ ( <stmnt>-from ) ]-str }|
             ).
          endloop.

        endif.

      when scan_struc_stmnt_type-protected_section or
          scan_struc_stmnt_type-private_section.
        " we are only interested in objects of the public section

      when scan_struc_stmnt_type-class_implementation.
        "we are not interested in the implementation of classes

    endcase.

  endmethod.



  method fill_tree.

    data(scan) = lcl_abapdocs_reposrc_scanner=>read_and_scan_code( class_pool_name ).


    data(pool_struc) = scan-structures[ 1 ].
    loop at scan-structures assigning field-symbol(<struc>)
        from pool_struc-struc_from
        to   pool_struc-struc_to
        where type = scan_struc_type-class or type = scan_struc_type-declaration or type = scan_struc_type-sequence.

      identify_comment_blocks(
          parent_stucture_id = 1
          structure_id = sy-tabix
          abapdocs_tree_parent_id = 0
          scan = scan
          scan_tree = scan_tree
      ).

    endloop.

  endmethod.

  method retrieve_abapdocs_for_stmt.

    data(abapdocs_regex) = new cl_abap_regex( '^\s*"!\s?(.*)$' ).

    data(index_in_tab_statements) = statement_id - 1.
    if index_in_tab_statements > 0 and
        ( scan-statements[ index_in_tab_statements ]-type = statement_type-comment_between or
          scan-statements[ index_in_tab_statements ]-type = statement_type-comment_within ).
      data(scan_statement) = scan-statements[ index_in_tab_statements ].

      data(token_id) = scan_statement-from.
      while token_id <= scan_statement-to.

        data(comment_line) = scan-tokens[ token_id ]-str.
        check comment_line is not initial. " like if and continue

        data(regex_matcher) = abapdocs_regex->create_matcher( text = comment_line ).
        if regex_matcher->find_next( ).
          append conv #( regex_matcher->get_submatch( 1 ) ) to r_result .
        endif.

        token_id = token_id + 1.
      endwhile.
    endif.
  endmethod.


  method read_and_scan_code.

    data scan_seo_section_source type seo_section_source.
    data replace_tab type sreptab.
    data program_name type program.
    data progname_part1 type char30.
    data error_msg type string.


    progname_part1 = class_pool_name && '=============================='.

    cl_abap_typedescr=>describe_by_name(
        exporting p_name = class_pool_name
        receiving p_descr_ref = data(typedescr)
        exceptions type_not_found = 1
                   others = 2 ).
    if sy-subrc <> 0.
      raise exception type lcx_error exporting message = |type "{ class_pool_name }" not found|.
    endif.

*    select progname from reposrc
    case typedescr->kind.
      when cl_abap_typedescr=>kind_class.
        program_name = progname_part1 && 'CP'.


        " exclude list for includes
        data    trdir_emtpy type trdir.
        data    source_emtpy  type scr_include.
        replace_tab = value #(
          ( name = progname_part1 && 'CCMAC'  source = ref #( source_emtpy ) trdir = ref #( trdir_emtpy ) ) " macros
          ( name = progname_part1 && 'CCAU'   source = ref #( source_emtpy ) trdir = ref #( trdir_emtpy ) ) " unit tests
          ( name = progname_part1 && 'CO'     source = ref #( source_emtpy ) trdir = ref #( trdir_emtpy ) ) " protected section
          ( name = progname_part1 && 'CI'     source = ref #( source_emtpy ) trdir = ref #( trdir_emtpy ) ) " private section
           ).
      when cl_abap_typedescr=>kind_intf.
        program_name = progname_part1 && 'IP'.
      when others.
        " unsupported
        raise exception type lcx_error exporting message = |class_pool_name "{ class_pool_name }" does not point to a global class or interface|.
    endcase.


    " read class-pool, only activated code
    read report program_name into scan_seo_section_source state 'A'.

    if sy-subrc <> 0.
      raise exception type lcx_error exporting message = |could not read program "{ program_name }"|.
    endif.

    scan abap-source scan_seo_section_source
            tokens          into scan-tokens
            statements      into scan-statements
            levels          into scan-levels
            structures      into scan-structures
            frame program   from program_name
*            include program from program_name
            with includes without trmac
            with comments
*            with declarations
*            with analysis
            replacing       replace_tab
            message into error_msg
            .

    if sy-subrc <> 0.
      raise exception type lcx_error exporting message = |could not scan program "{ program_name }". msg: "{ error_msg }"|.
    endif.

  endmethod.


  method gen_possible_include_prognames.
    " https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/index.htm?file=abapclass-pool.htm

*    select progname from reposrc

    " includes of class /WSV/CL_CA_SAF_ABAPDOCS_ANNOTS
*    /WSV/CL_CA_SAF_ABAPDOCS_ANNOTSCP       master program of the class
* +  /WSV/CL_CA_SAF_ABAPDOCS_ANNOTSCU       public declarations
*    /WSV/CL_CA_SAF_ABAPDOCS_ANNOTSCO       protected declarations
*    /WSV/CL_CA_SAF_ABAPDOCS_ANNOTSCI       private declarations

*    /WSV/CL_CA_SAF_ABAPDOCS_ANNOTSCCAU     unit test
* +  /WSV/CL_CA_SAF_ABAPDOCS_ANNOTSCCDEF    class-relevant local types
* +  /WSV/CL_CA_SAF_ABAPDOCS_ANNOTSCCIMP    local types
*    /WSV/CL_CA_SAF_ABAPDOCS_ANNOTSCCMAC    macro definitions

*    /WSV/CL_CA_SAF_ABAPDOCS_ANNOTSCM001    method implementation
*    /WSV/CL_CA_SAF_ABAPDOCS_ANNOTSCM002    method implementation
*    /WSV/CL_CA_SAF_ABAPDOCS_ANNOTSCM003    method implementation
*    /WSV/CL_CA_SAF_ABAPDOCS_ANNOTSCM004    method implementation
*    /WSV/CL_CA_SAF_ABAPDOCS_ANNOTSCM005    method implementation

*    /WSV/CL_CA_SAF_ABAPDOCS_ANNOTSCS       seems to be a preview/top100 of a class (new with nw 740 sp*)
*    /WSV/CL_CA_SAF_ABAPDOCS_ANNOTSCT       dummy include to reduce generation dependencies between ..

    " includes of interface /WSV/IF_CA_SAF_REST_CALL
*    /WSV/IF_CA_SAF_REST_CALL======IP       master program of the interface
*    /WSV/IF_CA_SAF_REST_CALL======IT       dummy include to reduce generation dependencies between ..
* +  /WSV/IF_CA_SAF_REST_CALL======IU       public declarations


    "\CLASS-POOL=ZCL_ACS_ABAPDOCS_CODE_SCANNER\CLASS=LTCL_CLASS_WITH_ABAP_DOCS
    if class_pool_name is not initial.
      data progname_part1 type char30.
      progname_part1 = class_pool_name && '=============================='.
      append progname_part1 && 'CU' to possible_includes.
      append progname_part1 && 'CCDEF' to possible_includes.
      append progname_part1 && 'CCIMP' to possible_includes.
      append progname_part1 && 'IU' to possible_includes.

    endif.
  endmethod.


endclass.

class lcx_error implementation.
  method constructor.
    super->constructor(
        previous = previous
    ).
    me->message = message.
  endmethod.
endclass.



*************************************************
* following code is just for the unit tests

class lcl_with_docs_v2 implementation.

  method meth1.

  endmethod.

  method meth2.

  endmethod.

  method constructor.

  endmethod.

endclass.

class ltcl_class_with_abap_docs implementation.

  method meth1.
  endmethod.

endclass.
