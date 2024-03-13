# abapdocs-code-scanner
scans class and interface definitions for abapDocs and offers the abapDocs text of public methods and type definitions

possible use-case
=================
abapDocs can be used to fill fields like description, enum, minimum, minItems, and deprecated in an OpenAPI specification (https://spec.openapis.org/).

example
=======
example code can be found in the unit test class ltcl_example of the global class zcl_acs_abapdocs_code_scanner

read abapDocs of a type definition
----------------------------------
    data person type zif_acs_abapdocs_example=>t_person.
    data(abapdocs_for_person) = zcl_acs_abapdocs_code_scanner=>create_by_typedescr( cl_abap_typedescr=>describe_by_data( person ) ).
    abapdocs_for_name->get_description( ).
read abapDocs of a method
-------------------------
    data(abapdocs_for_interface) = zcl_acs_abapdocs_code_scanner=>create_by_typedescr( cl_abap_objectdescr=>describe_by_name( 'ZIF_ACS_ABAPDOCS_EXAMPLE' ) ).
    data(abapdocs_for_method_list) = abapdocs_for_interface->get_abapdocs_for_child( 'list' ).
    abapdocs_for_method_list->get_description( ).
read abapDocs of a constant structure
-------------------------------------
One use case is to create dynamic search help with description fields filled by abapDocs

### definition of the logical enumeration (pre 7.52)

    interface zif_example_types
      public .
    
      constants:
        "! pre ABAP 7.52 enumeration
        begin of msg_type,
          "! error message
          error   type string value `error`,
          "! warning message
          warning type string value `warning`,
          "! info message
          info    type string value `info`,
        end of msg_type.
    
    endinterface.

### build itab

note: Method `create_by_typedescr` needs additionally the `absolute_name`, because the structure definition (`desc`) points to a dynamic type name (`\TYPE=%_T000...`). The abapdocs-code-scanner needs to know the code object where it can find the structure and its abapDocs. 

    types: begin of ty_line,
             value         type string,
             description type string,
           end of ty_line.
    " itab that can be used for function F4UT_PARAMETER_RESULTS_PUT
    data source_tab type standard table of ty_line with empty key.
    
    field-symbols <value> type  string.
    
    data(desc) = cast cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( zif_example_types=>msg_type ) ).
    data(acs) = zcl_acs_abapdocs_code_scanner=>create_by_typedescr(
          typedescr = desc
          absolute_name = '\INTERFACE=ZIF_EXAMPLE_TYPES\TYPE=MSG_TYPE' ).
    loop at desc->get_components( ) assigning field-symbol(<comp>).
      assign component <comp>-name of structure zif_example_types=>msg_type to <value>.
      append value #( value = <value> description = acs->get_abapdocs_for_child( <comp>-name )->get_description( ) )
             to source_tab.
    endloop.
