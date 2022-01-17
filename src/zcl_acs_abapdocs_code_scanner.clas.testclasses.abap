class ltcl_example definition for testing duration short risk level harmless.
  private section.
    methods read_abapdocs_for_structure for testing.
    methods read_abapdocs_for_method for testing.
endclass.

class ltcl_example implementation.

  method read_abapdocs_for_structure.
    data person type zif_acs_abapdocs_example=>t_person.
    data(abapdocs_for_person) = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>create_by_typedescr( cl_abap_typedescr=>describe_by_data( person ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = `data of a person`
        act = abapdocs_for_person->get_description( )
     ).

    data(abapdocs_for_name) = abapdocs_for_person->get_abapdocs_for_child( 'name' ).
    cl_abap_unit_assert=>assert_equals(
        exp = `name of the person`
        act = abapdocs_for_name->get_description( )
     ).

    data(abapdocs_for_gender) = abapdocs_for_person->get_abapdocs_for_child( 'gender' ).
    cl_abap_unit_assert=>assert_equals(
        exp = value zif_acs_abapdocs_annotation=>t_value_list( ( `female` ) ( `male` ) ( `undefined` ) )
        act = abapdocs_for_gender->get_annotations( )->get_value_as_list( 'enum' )
     ).

    data(abapdocs_for_address) = abapdocs_for_person->get_abapdocs_for_child( 'address' ).
    cl_abap_unit_assert=>assert_equals(
        exp = `address of the person`
        act = abapdocs_for_address->get_description( )
     ).

    data(abapdocs_for_street) = abapdocs_for_address->get_abapdocs_for_child( 'street' ).
    cl_abap_unit_assert=>assert_equals(
        exp = `the street`
        act = abapdocs_for_street->get_description( )
     ).

    data(abapdocs_for_old_street) = abapdocs_for_address->get_abapdocs_for_child( 'old_street' ).
    cl_abap_unit_assert=>assert_true(
        act = abapdocs_for_old_street->get_annotations( )->has_key( 'deprecated' )
     ).

  endmethod.

  method read_abapdocs_for_method.
    data(abapdocs_for_interface) = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>create_by_typedescr( cl_abap_objectdescr=>describe_by_name( 'ZIF_ACS_ABAPDOCS_EXAMPLE' ) ).

    cl_abap_unit_assert=>assert_char_cp(
        exp = `is used to demonstrate*`
        act = abapdocs_for_interface->get_description( )
     ).

    data(abapdocs_for_method_list) = abapdocs_for_interface->get_abapdocs_for_child( 'list' ).
    cl_abap_unit_assert=>assert_equals(
        exp = `returns a list of people`
        act = abapdocs_for_method_list->get_description( )
     ).
  endmethod.

endclass.

class ltcl_unittest definition for testing duration short risk level harmless.
  private section.
    methods read_abapdocs_from_meth1 for testing.
    methods read_abapdocs_from_stru1 for testing.
    methods read_abapdocs_sub_struct for testing.
    methods read_abapdocs_from_stru1_multi for testing.

    methods read_abapdocs_for_literal for testing.
    methods read_abapdocs_from_stru_global for testing.
endclass.

class ltcl_unittest_internal definition for testing duration short risk level harmless.
  private section.

    methods split_into_annoation_key_value for testing.

    methods get_possible_includes for testing.
    methods get_fields_with_annotation for testing.
    methods get_path_for_typedescr for testing.
    methods to_debug for testing.
endclass.

class ltcl_unittest implementation.

  method read_abapdocs_from_meth1.
    data(class_desc) = cast cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref( new ltcl_class_with_abap_docs(  ) ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 'abap_docs1'
        act = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>create_by_typedescr( class_desc )->get_abapdocs_for_child( `METH1` )->get_description( ) ).

    class_desc = cast cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref( new lcl_with_docs_v2(  ) ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 'meth1-docs'
        act = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>create_by_typedescr( class_desc )->get_abapdocs_for_child( `METH1` )->get_description( ) ).
  endmethod.

  method read_abapdocs_from_stru1.
    data test_struct type t_abapdocs_test.
    data(type_desc) = cl_abap_structdescr=>describe_by_data( test_struct ).
    data(abap_docs_for_struct) = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>create_by_typedescr( type_desc ).
    cl_abap_unit_assert=>assert_equals(
        exp = |f2-docs|
        act = abap_docs_for_struct->get_abapdocs_for_child( `F2` )->get_description( ) ).

    data(abap_docs_of_sub_table) = abap_docs_for_struct->get_abapdocs_for_child( `SUB_TABLE` ).
    cl_abap_unit_assert=>assert_equals(
        exp = |sub_table-docs|
        act = abap_docs_of_sub_table->get_description( ) ).

    cl_abap_unit_assert=>assert_equals(
        exp = |doc of test_sub|
        act = abap_docs_of_sub_table->get_abapdocs_for_child( 'table_line' )->get_description( ) ).

    data(typedesc_3) = cl_abap_structdescr=>describe_by_name( 'T_ABAPDOCS_TEST_SUB' ).
    cl_abap_unit_assert=>assert_equals(
        exp = |sub1-docs|
        act = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>create_by_typedescr( typedesc_3
                                                            )->get_abapdocs_for_child( `SUB1` )->get_description( ) ).

  endmethod.

  method read_abapdocs_sub_struct.
    data test_struct type t_abapdocs_test.
    data(type_desc) = cl_abap_structdescr=>describe_by_data( test_struct ).
    data(abap_docs_for_struct) = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>create_by_typedescr( type_desc ).

    data(abap_docs_stru_1) = abap_docs_for_struct->get_abapdocs_for_child( `STRU_1` ).
    data(abap_docs_s1_1) = abap_docs_stru_1->get_abapdocs_for_child( `S1_1` ).

    cl_abap_unit_assert=>assert_equals(
       exp = |s1_1-docs|
       act = abap_docs_s1_1->get_description( )
    ).
  endmethod.

  method read_abapdocs_from_stru1_multi.
    data test_struct type t_abapdocs_test.
    data(type_desc) = cl_abap_structdescr=>describe_by_data( test_struct ).
    data(abap_docs_for_struct) = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>create_by_typedescr( type_desc ).
    data(annotations) = abap_docs_for_struct->get_abapdocs_for_child( 'f1' )->get_annotations( ).
    cl_abap_unit_assert=>assert_equals(
        exp = `["types", "methods", "class"]`
        act = annotations->get_value( 'enum' ) ).

    data(abap_docs_for_stru_1) = abap_docs_for_struct->get_abapdocs_for_child( 'stru_1' ).
    cl_abap_unit_assert=>assert_equals(
        exp = |stru_1-docs|
        act = abap_docs_for_stru_1->get_description( )
    ).

  endmethod.

  method read_abapdocs_for_literal.
    data test_literal type ltcl_class_with_abap_docs=>t_literal.
    data(type_desc) = cl_abap_typedescr=>describe_by_data( test_literal ).
    data(abap_docs_for_literal) = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>create_by_typedescr( type_desc ).
    cl_abap_unit_assert=>assert_equals(
        exp = |only a literal|
        act = abap_docs_for_literal->get_description( )
        ).
  endmethod.

  method read_abapdocs_from_stru_global.
    data test_struct type zcl_acs_abapdocs_annotation=>t_key_value.
    data(type_desc) = cl_abap_typedescr=>describe_by_data( test_struct ).
    data(abap_docs_for_literal) = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>create_by_typedescr( type_desc ).
    cl_abap_unit_assert=>assert_equals(
        exp = |annotation|
        act = abap_docs_for_literal->get_description( )
        ).
  endmethod.

endclass.

class ltcl_unittest_internal implementation.
  method split_into_annoation_key_value.

    " description line
    cl_abap_unit_assert=>assert_initial(
        act = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>split_into_annoation_key_value( 'minItems: 1' )
    ).

    cl_abap_unit_assert=>assert_equals(
        exp = value zcl_acs_abapdocs_annotation=>t_key_value( key = 'minItems' value = '1' )
        act = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>split_into_annoation_key_value( '@minItems: 1' )
    ).

    cl_abap_unit_assert=>assert_equals(
        exp = value zcl_acs_abapdocs_annotation=>t_key_value( key = 'required' value = '' )
        act = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>split_into_annoation_key_value( '@required' )
    ).

    cl_abap_unit_assert=>assert_equals(
        exp = value zcl_acs_abapdocs_annotation=>t_key_value( key = 'enum' value = '["customer", "provisioner", "employee", "contact"]' )
        act = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>split_into_annoation_key_value( '@enum: ["customer", "provisioner", "employee", "contact"]' )
    ).

    cl_abap_unit_assert=>assert_equals(
        exp = value zcl_acs_abapdocs_annotation=>t_key_value( key = 'parameter' value = 'name      | doc of param name' )
        act = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>split_into_annoation_key_value( '@parameter name      | doc of param name' )
    ).

  endmethod.


  method get_possible_includes.
    data(includes) = lcl_abapdocs_reposrc_scanner=>gen_possible_include_prognames( class_pool_name = 'ZCL_ACS_ABAPDOCS_CODE_SCANNER'  ).

    sort includes.
    cl_abap_unit_assert=>assert_equals(
        exp = value  lcl_abapdocs_reposrc_scanner=>t_possible_includes(
                ( 'ZCL_ACS_ABAPDOCS_CODE_SCANNER=CCDEF' )
                ( 'ZCL_ACS_ABAPDOCS_CODE_SCANNER=CCIMP' )
                ( 'ZCL_ACS_ABAPDOCS_CODE_SCANNER=CU' )
                ( 'ZCL_ACS_ABAPDOCS_CODE_SCANNER=IU' )
            )
        act = includes
        ).
  endmethod.

  method get_fields_with_annotation.
    data test_struct type t_abapdocs_test.
    data(type_desc) = cl_abap_structdescr=>describe_by_data( test_struct ).
    data(abap_docs_for_struct) = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>create_by_typedescr( type_desc ).

    data(name_mappings) = abap_docs_for_struct->get_fields_with_annotation( 'jsonName' ).

    cl_abap_unit_assert=>assert_table_contains(
        line             = value /ui2/cl_json=>name_mapping( abap = 'F1' json = 'field1' )
        table            = name_mappings
    ).
    cl_abap_unit_assert=>assert_table_contains(
        line             = value /ui2/cl_json=>name_mapping( abap = 'F2' json = 'field2' )
        table            = name_mappings
    ).
  endmethod.

  method get_path_for_typedescr.

    cl_abap_unit_assert=>assert_equals(
        exp = value lif_abapdocs_scan_tree=>t_path( ( `zif_acs_abapdocs_example` ) )
        act = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>get_path_for_typedescr( '\\INTERFACE=ZIF_ACS_ABAPDOCS_EXAMPLE  ' )
        ).

    cl_abap_unit_assert=>assert_equals(
        exp = value lif_abapdocs_scan_tree=>t_path( ( `zif_acs_abapdocs_example` ) ( `t_person` ) )
        act = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>get_path_for_typedescr( '\\INTERFACE=ZIF_ACS_ABAPDOCS_EXAMPLE\\TYPE=T_PERSON  ' )
        ).

    cl_abap_unit_assert=>assert_equals(
        exp = value lif_abapdocs_scan_tree=>t_path( ( `ltcl_class_with_abap_docs` ) ( `t_literal` ) )
        act = ZCL_ACS_ABAPDOCS_CODE_SCANNER=>get_path_for_typedescr( '\\CLASS-POOL=ZCL_ACS_ABAPDOCS_CODE_SCANNER\\CLASS=LTCL_CLASS_WITH_ABAP_DOCS\\TYPE=T_LITERAL ' )
        ).


  endmethod.

  METHOD to_debug.
    data(abapdocs_scan_tree) = new lcl_abapdocs_scan_tree( ).
    lcl_abapdocs_reposrc_scanner=>fill_tree(
        class_pool_name = 'ZCL_ACS_ABAPDOCS_CODE_SCANNER'
        scan_tree       = abapdocs_scan_tree
    ).
  ENDMETHOD.

endclass.
