"! annotation for a specific key not found
class zcx_acs_annotation_not_found definition
  public
  inheriting from cx_no_check
  final
  create public .

  public section.

    interfaces if_t100_message .

    methods constructor
      importing
        key type string .

    data key type string read-only.
  protected section.
  private section.
endclass.



class zcx_acs_annotation_not_found implementation.


  method constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ).
    me->key = key.
  endmethod.
endclass.
