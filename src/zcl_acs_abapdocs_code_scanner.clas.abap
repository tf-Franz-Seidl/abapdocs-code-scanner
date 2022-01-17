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

    "! get all fields that have the specific annotation of one scanned source code object (class/interface) <br/>
    "! result: table: field-name : annotation-value <br/>
    "! e.g. used for retrieving jsonName in runtime
    methods get_fields_with_annotation
      importing
                field_keyword        type string default 'TYPES'
                annotation_key       type string
      returning value(name_mappings) type /ui2/cl_json=>name_mappings.
  protected section.

  private section.
    types:
      begin of t_annotation_and_description,
        annotation_pairs type zcl_acs_abapdocs_annotation=>t_key_values,
        description      type string,
      end of t_annotation_and_description,
      begin of t_scan_cache,
        class_pool_name type string,
        abapdocs_tree   type ref to lif_abapdocs_scan_tree,
      end of t_scan_cache
      .


    class-data scan_cache type hashed table of t_scan_cache with unique key class_pool_name.
    class-data annotation_regex type ref to cl_abap_regex.

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
    class-methods extract_annotations_and_descr
      importing docs_lines           type lif_abapdocs_scan_tree=>t_abapdocs_source_lines
      returning value(anno_and_desc) type t_annotation_and_description.

    class-methods get_path_for_typedescr
      importing absolute_name type abap_abstypename
      returning value(path)   type lif_abapdocs_scan_tree=>t_path.



    class-methods extract_class_pool_name
      importing
        i_absolute_name          type abap_abstypename
      returning
        value(r_class_pool_name) type string
      raising
        cx_sy_matcher
        cx_sy_regex .

    class-methods split_into_annoation_key_value
      importing docs_line         type seo_section_source_line
      returning value(annotation) type zcl_acs_abapdocs_annotation=>t_key_value.
ENDCLASS.



CLASS ZCL_ACS_ABAPDOCS_CODE_SCANNER IMPLEMENTATION.


  method constructor.

    me->abapdocs_tree = abapdocs_tree.
    me->path = path.
    me->typedescr     = typedescr.

*      data(annotation_and_description) = extract_annoations_and_descr( abapdocs_tree->get_abapdocs_for_path( path ) ).
    me->annotations =  new zcl_acs_abapdocs_annotation( annotation_and_description-annotation_pairs ).
    me->description = annotation_and_description-description.
    condense me->description.

  endmethod.


  method create_by_typedescr.

    data(class_pool_name) = zcl_acs_abapdocs_code_scanner=>extract_class_pool_name( typedescr->absolute_name ).
    data(path) = get_path_for_typedescr( typedescr->absolute_name  ).

    if class_pool_name is not initial.
      try.
          if not line_exists( scan_cache[ class_pool_name = class_pool_name ] ).
            data(abapdocs_scan_tree) = new lcl_abapdocs_scan_tree( ).
            lcl_abapdocs_reposrc_scanner=>fill_tree(
                scan_tree = abapdocs_scan_tree
                class_pool_name = class_pool_name
                ).
            insert value #( class_pool_name = class_pool_name abapdocs_tree = abapdocs_scan_tree ) into table scan_cache.
          endif.

          data(abapdocs_tree) = scan_cache[ class_pool_name = class_pool_name ]-abapdocs_tree.
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
      data(annotation) = split_into_annoation_key_value( docs_line ).
      if annotation is not initial.
        " line includes an "@" --> new annotation
        if not line_exists( annotation_key_values[ key = annotation-key ] ). " annotation keys should be unique, in case of multiple occurrence the first one wins
          insert annotation into table annotation_key_values assigning field-symbol(<last_annotation>).
        endif.
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


  method extract_class_pool_name.
    " eg: \CLASS-POOL=ZCL_ACS_ABAPDOCS_CODE_SCANNER\CLASS=LTCL_CLASS_WITH_ABAP_DOCS\TYPE=T_LITERAL
    data(matcher) = cl_abap_matcher=>create(
        pattern       =  '^\\(CLASS|INTERFACE)(-POOL)?=([A-Z0-9_/]*)(\\.*)?$'
        text          =  i_absolute_name
    ).
    if matcher->find_next( ).
      r_class_pool_name  = matcher->get_submatch( 3 ).
    endif.

  endmethod.


  method get_abapdocs_for_child.

    data childs_typedescr type ref to cl_abap_typedescr.

    data(childs_path) = value #( base me->path ( |{ name case = lower }| ) ).

    " try to retrieve typedescr of child
    if me->typedescr is bound.
      case cl_abap_classdescr=>get_class_name( me->typedescr ).
        when '\CLASS=CL_ABAP_STRUCTDESCR'.
          if me->typedescrs_of_childs is initial.
            me->typedescrs_of_childs = cast cl_abap_structdescr( me->typedescr )->get_symbols( ).
          endif.
          if line_exists( me->typedescrs_of_childs[ name = name ] ).
            childs_typedescr = me->typedescrs_of_childs[ name = name ]-type.
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


  method get_fields_with_annotation.
    name_mappings = me->abapdocs_tree->get_fields_with_annotation(
          field_keyword  = field_keyword
          annotation_key = annotation_key
     ).
  endmethod.


  method get_path_for_typedescr.
    " eg: \CLASS-POOL=ZCL_ACS_ABAPDOCS_CODE_SCANNER\CLASS=LTCL_CLASS_WITH_ABAP_DOCS\TYPE=T_LITERAL

    split |{ condense( absolute_name ) }| at '\' into table data(parts).

    loop at parts into data(part)
        where table_line is not initial.

      split part at '=' into data(type) data(name).
      if sy-subrc = 0 and type <> 'CLASS-POOL'.
        append |{ name case = lower }| to path.
      endif.
    endloop.

  endmethod.


  method get_typedescr.
    typedescr = me->typedescr.
  endmethod.


  method split_into_annoation_key_value.
    if annotation_regex is not bound.
      annotation_regex = new #( '\s*@(\w{3,})\s*(:)?\s*(.*)?' ).
    endif.

    data(matcher) = annotation_regex->create_matcher( text = docs_line ).

    if matcher->find_next( ).
      annotation = value #( key = matcher->get_submatch( 1 ) value = matcher->get_submatch( 3 ) ).
    endif.
  endmethod.
ENDCLASS.
