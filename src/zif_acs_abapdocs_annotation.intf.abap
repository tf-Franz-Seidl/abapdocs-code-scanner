interface zif_acs_abapdocs_annotation
  public .

  types t_value_list type standard table of string with empty key.

  types t_key_list type sorted table of string with unique key table_line.

  methods has_key
    importing key           type string
    returning value(exists) type abap_bool.

  methods get_keys
    returning value(keys) type t_key_list.

  "! raise zcx_tf_abapdocs_anno_not_found if not found
  methods get_value
    importing key          type string
    returning value(value) type string.

  "! for keys that can have more than one value
  "! eg. parameter (of methods)
  methods get_values
    importing key           type string
    returning value(values) type zif_acs_abapdocs_annotation=>t_value_list.

  "! used eg for annotations (@enum: [ "off", "on" ])
  methods get_value_as_list
    importing key               type string
    returning value(value_list) type zif_acs_abapdocs_annotation=>t_value_list.

endinterface.
