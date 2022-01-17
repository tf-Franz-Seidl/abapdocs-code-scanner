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
