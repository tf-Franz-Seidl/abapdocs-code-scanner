class zcl_acs_abapdocs_annotation definition
  public
  final
  create public .

  public section.
    interfaces zif_acs_abapdocs_annotation.
    types:
      "! annotation
      begin of t_key_value,
        "! keyword of annotation (e.g. 'minItems')
        key   type string,
        "! value of annotation
        value type string,
      end of t_key_value.
    "! key value list: at for annotation parameter the list must be non-unique
    types t_key_values type sorted table of t_key_value with non-unique key key.

    methods constructor
      importing annotations type t_key_values.
  protected section.
    data annotations type t_key_values.
  private section.
endclass.



class zcl_acs_abapdocs_annotation implementation.


  method constructor.
    me->annotations = annotations.
  endmethod.


  method zif_acs_abapdocs_annotation~get_keys.
    loop at me->annotations assigning field-symbol(<anno>).
      insert <anno>-key into table keys. " ignore sy-subrc / duplicates (e.g. multiple parameter declerations)
    endloop.
  endmethod.


  method zif_acs_abapdocs_annotation~get_value.
    try.
        value = annotations[ key = key ]-value.
      catch cx_sy_itab_line_not_found into data(lx_not_found).
        if value_if_initial is supplied.
          value = value_if_initial.
        else.
          raise exception type zcx_acs_annotation_not_found
            exporting
              key = key.
        endif.
    endtry.
  endmethod.


  method zif_acs_abapdocs_annotation~get_values.
    loop at me->annotations assigning field-symbol(<key_value>) where key = key.
      append <key_value>-value to values.
    endloop.
  endmethod.


  method zif_acs_abapdocs_annotation~get_value_as_list.
    data(enum_matcher) = new cl_abap_regex( '"([^"]{1,})"' )->create_matcher(
            text = me->zif_acs_abapdocs_annotation~get_value( key ) ).

    while enum_matcher->find_next( ).
      append enum_matcher->get_submatch( 1 ) to value_list.
    endwhile.
  endmethod.


  method zif_acs_abapdocs_annotation~has_key.
    exists = xsdbool( line_exists( annotations[ key = key ] ) ).
  endmethod.
endclass.
