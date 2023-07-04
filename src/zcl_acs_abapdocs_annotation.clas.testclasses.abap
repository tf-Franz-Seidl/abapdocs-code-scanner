class lcl_test definition for testing duration short risk level harmless.
  private section.
    methods setup.
    methods test_get_values for testing.
    methods test_get_value for testing.
    methods test_get_value_nf for testing.
    methods test_get_values_nf for testing.
    methods test_get_value_as_list_nf for testing.
    methods test_get_value_as_list0 for testing.
    methods test_get_value_as_list1 for testing.
    methods test_get_value_as_list2 for testing.
    methods test_get_value_as_list3 for testing.
    methods has_key for testing.
    methods has_key_nf for testing.
    methods get_keys for testing.

    data anno type ref to zif_acs_abapdocs_annotation.

endclass.

class lcl_test implementation.

  method test_get_values.

    cl_abap_unit_assert=>assert_table_contains(
        line = `param desc 1`
        table = anno->get_values( `parameter` )
    ).

    cl_abap_unit_assert=>assert_table_contains(
        line = `param desc 2`
        table = anno->get_values( `parameter` )
    ).
  endmethod.

  method setup.
    me->anno = new ZCL_ACS_ABAPDOCS_ANNOTATION( value #(
        ( key = `enum0` value = `[ ]`  )
        ( key = `enum1` value = `[ "off" ]`  )
        ( key = `enum2` value = `[ "off", "on" ]`  )
        ( key = `enum3` value = `[ "off", "on", "other" ]`  )
        ( key = `parameter` value = `param desc 1` )
        ( key = `parameter` value = `param desc 2` )
        ( key = `key1` value = `desc 1` )
     ) ).
  endmethod.

  method test_get_value.
    cl_abap_unit_assert=>assert_equals(
        exp = `desc 1`
        act = anno->get_value( `key1` )
    ).
  endmethod.

  method test_get_value_nf.
    try.
        anno->get_value( `key_that_s_not_there` ).
        cl_abap_unit_assert=>fail( |excption should be thrown| ).
      catch zcx_acs_annotation_not_found.
        " should be raised
    endtry.
  endmethod.


  method test_get_values_nf.
    cl_abap_unit_assert=>assert_initial(
        anno->get_values( `key_that_s_not_there` )
        ).
  endmethod.

  method test_get_value_as_list_nf.
    try.
        anno->get_value_as_list( `key_that_s_not_there` ).
        cl_abap_unit_assert=>fail( |excption should be thrown| ).
      catch zcx_acs_annotation_not_found.
        " should be raised
    endtry.
  endmethod.

  method test_get_value_as_list0.
    cl_abap_unit_assert=>assert_equals(
        exp = value zif_acs_abapdocs_annotation=>t_value_list( )
        act = anno->get_value_as_list( `enum0` )
    ).
  endmethod.
  method test_get_value_as_list1.
    cl_abap_unit_assert=>assert_equals(
        exp = value zif_acs_abapdocs_annotation=>t_value_list( ( `off` ) )
        act = anno->get_value_as_list( `enum1` )
    ).
  endmethod.
  method test_get_value_as_list2.
    cl_abap_unit_assert=>assert_equals(
        exp = value zif_acs_abapdocs_annotation=>t_value_list( ( `off` ) ( `on`) )
        act = anno->get_value_as_list( `enum2` )
    ).
  endmethod.
  method test_get_value_as_list3.
    cl_abap_unit_assert=>assert_equals(
        exp = value zif_acs_abapdocs_annotation=>t_value_list( ( `off` ) ( `on`) ( `other` ) )
        act = anno->get_value_as_list( `enum3` )
    ).
  endmethod.



  method has_key.
    cl_abap_unit_assert=>assert_true(
            act = anno->has_key( `enum3` )
        ).
  endmethod.

  method has_key_nf.
    cl_abap_unit_assert=>assert_false(
                act = anno->has_key( `nothing` )
            ).
  endmethod.

  METHOD get_keys.
    data(keys) = anno->get_keys( ).
  ENDMETHOD.

endclass.
