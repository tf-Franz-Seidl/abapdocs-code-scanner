"! scans code of class/interfaces to read abapDocs of public method/type descriptions<br/>
"! for examples look at the unit test class ltcl_example
class zcl_acs_abapdocs_code_scanner definition
  public
  final
  create private .

  public section.

    "! factory method <br/>
    "! create a code scanner instance for a class/interface/type descriptor<br/>
    "! in error case, returns either an empty tree or a null reference
    class-methods create_by_typedescr
      importing typedescr                      type ref to cl_abap_typedescr
                create_empty_instance_on_error type abap_bool default abap_true
      returning value(instance)                type ref to zcl_acs_abapdocs_code_scanner.

    "! get annotations included in the abapDocs of the current method/field
    methods get_annotations
      returning value(annotations) type ref to zif_acs_abapdocs_annotation.

    "! get text of abapDocs excluding annotations of the current method/field
    methods get_description returning value(description) type string.

    "! get abapDocs scan object for a type description
    methods get_abapdocs_for_child
      importing name                      type string
      returning value(abapdocs_for_child) type ref to zcl_acs_abapdocs_code_scanner.

    methods get_typedescr
      returning value(typedescr) type ref to cl_abap_typedescr.

    "! copy from cl_ui2_json for backwards compatibility of old 7.40/7.50 systems
    types json type string .
    types:
      begin of name_mapping,
        abap type abap_compname,
        json type string,
      end of name_mapping .
    types:
      name_mappings type hashed table of zcl_acs_abapdocs_code_scanner=>name_mapping with unique key abap .

    "! get all fields that have the specific annotation of one scanned source code object (class/interface) <br/>
    "! result: table: field-name : annotation-value <br/>
    "! e.g. used for retrieving jsonName in runtime
    methods get_fields_with_annotation
      importing
                field_keyword        type string default 'TYPES'
                annotation_key       type string
      returning value(name_mappings) type zcl_acs_abapdocs_code_scanner=>name_mappings.


    methods get_path returning value(path) type string.

    types: t_error_message  type string,
           t_error_messages type standard table of t_error_message with empty key.

    methods get_errors
      returning value(error_messages) type t_error_messages.

  protected section.


  private section.
    types:
      begin of t_annotation_and_description,
        annotation_pairs type zcl_acs_abapdocs_annotation=>t_key_values,
        description      type string,
      end of t_annotation_and_description.


    data abapdocs_tree type ref to lif_abapdocs_scan_tree.
    data path type lif_abapdocs_scan_tree=>t_path.
    data typedescr type ref to cl_abap_typedescr.
    data typedescrs_of_childs type abap_component_symbol_tab.

    "! load on demand (like a cache)
    data annotations type ref to zif_acs_abapdocs_annotation.
    data description type string.


    methods constructor
      importing
        abapdocs_tree              type ref to lif_abapdocs_scan_tree
        path                       type lif_abapdocs_scan_tree=>t_path
        typedescr                  type ref to cl_abap_typedescr
        annotation_and_description type t_annotation_and_description
      .

    class-data annotation_regex type ref to cl_abap_regex.
    class-methods extract_annotations_and_descr
      importing docs_lines           type lif_abapdocs_scan_tree=>t_abapdocs_source_lines
      returning value(anno_and_desc) type t_annotation_and_description.

ENDCLASS.



CLASS ZCL_ACS_ABAPDOCS_CODE_SCANNER IMPLEMENTATION.


  method constructor.

    me->abapdocs_tree = abapdocs_tree.
    me->path = path.
    me->typedescr     = typedescr.

    me->annotations =  new zcl_acs_abapdocs_annotation( annotation_and_description-annotation_pairs ).
    me->description = condense( annotation_and_description-description ).

  endmethod.


  method create_by_typedescr.

    data(class_pool_name) = lcl_extractor=>extract_class_pool_name( typedescr->absolute_name ).
    data(path) = lcl_extractor=>get_path_for_typedescr( typedescr->absolute_name  ).

    if class_pool_name is not initial.
      try.
          data(abapdocs_tree) = lcl_scan_repo=>get_abapdocs_tree( class_pool_name ).
          instance = new #(
            abapdocs_tree = abapdocs_tree
            path = path
            typedescr = typedescr
            annotation_and_description = extract_annotations_and_descr( abapdocs_tree->get_abapdocs_for_path( path ) )
             ).

        catch lcx_error into data(lx_error).
          if create_empty_instance_on_error = abap_true.
            " create a dummy/empty instance
            instance = new #( abapdocs_tree = new lcl_abapdocs_scan_tree( ) path = value #(  ) typedescr = typedescr annotation_and_description = value #( ) ).
          endif.
      endtry.
    else.
      if create_empty_instance_on_error = abap_true.
        " create a dummy/empty instance
        instance = new #( abapdocs_tree = new lcl_abapdocs_scan_tree( ) path = value #(  ) typedescr = typedescr annotation_and_description = value #( ) ).
      endif.
    endif.
  endmethod.


  method extract_annotations_and_descr.
    " conditions: description text is always before annotations
    " description is not allowed after an annotation
    " --> all lines after an annotations belongs to that annotation

    data description_lines type standard table of string.
    data annotation_key_values type zcl_acs_abapdocs_annotation=>t_key_values.

    loop at docs_lines into data(docs_line).
      data(annotation) = lcl_extractor=>split_into_annoation_key_value( docs_line ).
      if annotation is not initial.
        " line includes an "@" --> new annotation
        insert annotation into table annotation_key_values assigning field-symbol(<last_annotation>).
      else.
        if <last_annotation> is assigned.
          " line belongs to the annotation above
          <last_annotation>-value = |{ <last_annotation>-value } { condense( docs_line ) }|.
        else.
          " no annotation so far --> it's an description
          append docs_line to description_lines.
        endif.
      endif.
    endloop.


    anno_and_desc = value #(
            annotation_pairs = annotation_key_values
            description = reduce string( init text = `` sep = ``
                                    for desc_line in description_lines
                                    next text = |{ text }{ sep }{ condense( desc_line ) }| sep = ` ` )
            ).
  endmethod.


  method get_abapdocs_for_child.

    data childs_typedescr type ref to cl_abap_typedescr.

    data(childs_path) = value #( base me->path ( |{ name case = lower }| ) ).
    data(name_capitalize) = |{ name case = upper }|.

    " try to retrieve typedescr of child
    if me->typedescr is bound.
      case cl_abap_classdescr=>get_class_name( me->typedescr ).
        when '\CLASS=CL_ABAP_STRUCTDESCR'.
          if me->typedescrs_of_childs is initial.
            me->typedescrs_of_childs = cast cl_abap_structdescr( me->typedescr )->get_symbols( ).
          endif.
          if line_exists( me->typedescrs_of_childs[ name = name_capitalize ] ).
            childs_typedescr = me->typedescrs_of_childs[ name = name_capitalize ]-type.
          endif.

        when '\CLASS=CL_ABAP_TABLEDESCR'.
          childs_typedescr = cast cl_abap_tabledescr( me->typedescr )->get_table_line_type( ).
          if childs_typedescr->kind = cl_abap_typedescr=>kind_ref.
            childs_typedescr = cast cl_abap_refdescr( childs_typedescr )->get_referenced_type( ).
          endif.

          abapdocs_for_child = zcl_acs_abapdocs_code_scanner=>create_by_typedescr( childs_typedescr ).

          return.

        when '\CLASS=CL_ABAP_CLASSDESCR' or
             '\CLASS=CL_ABAP_INTFDESCR'.
          cast cl_abap_objectdescr( me->typedescr )->get_attribute_type(
            exporting
              p_name              = name
            receiving
              p_descr_ref         = childs_typedescr
            exceptions
              attribute_not_found = 1
              others              = 2
          ).
*          if sy-subrc <> 0.
          " error is handled below with "childs_typedescr is bound"
*          endif.

      endcase.

    endif.

    data(annot_and_desc_of_field) = extract_annotations_and_descr( abapdocs_tree->get_abapdocs_for_path( childs_path ) ).

    " type of data element or structure
    " in case of a sub structure in a type definition abapdocs_of_type is null
    if childs_typedescr is bound
        and not childs_typedescr->is_ddic_type( )
*        and cl_abap_classdescr=>get_class_name( childs_typedescr ) <> '\CLASS=CL_ABAP_ELEMDESCR'
        .

      data(abapdocs_of_childs_type) = zcl_acs_abapdocs_code_scanner=>create_by_typedescr( typedescr = childs_typedescr create_empty_instance_on_error = abap_false ).

    endif.


    if abapdocs_of_childs_type is bound. " null e.g. in case of local type-descr like "\TYPE=%_T00004S00000048O0000089440" or "\TYPE=STRING"
      abapdocs_for_child = abapdocs_of_childs_type.

      abapdocs_for_child->annotations = new zcl_acs_abapdocs_annotation( value #(
                base annot_and_desc_of_field-annotation_pairs
                for key in abapdocs_for_child->annotations->get_keys( ) ( key = key value = abapdocs_for_child->annotations->get_value( key ) ) ) ).

      if annot_and_desc_of_field-description is not initial.
        abapdocs_for_child->description = annot_and_desc_of_field-description.
      endif.

    else.

      abapdocs_for_child = new #(
          abapdocs_tree = me->abapdocs_tree
          path          = childs_path
          typedescr     = childs_typedescr
          annotation_and_description = value #(
              annotation_pairs  = annot_and_desc_of_field-annotation_pairs
              description       = annot_and_desc_of_field-description
          )
      ).

    endif.
  endmethod.


  method get_annotations.
    annotations = me->annotations.
  endmethod.


  method get_description.
    description = me->description.
  endmethod.


  method get_errors.
    me->abapdocs_tree->get_errors( ).
  endmethod.


  method get_fields_with_annotation.
    name_mappings = me->abapdocs_tree->get_fields_with_annotation(
          field_keyword  = field_keyword
          annotation_key = annotation_key
     ).
  endmethod.


  method get_path.
    path = reduce #( init p = `` for line in me->path next p = p && '/' && line ).
  endmethod.


  method get_typedescr.
    typedescr = me->typedescr.
  endmethod.
ENDCLASS.
