"! is used to demonstrate the usage of abapDocs Code Scanner<br/>
"! the type declaration and the method are used in example code pieces<br/>
"! have a look to the unit test of zcl_tf_abapdocs_code_scanner
interface zif_acs_abapdocs_example
  public .

  types:
    "! data of a person
    begin of t_person,
      "! name of the person
      name   type string,
      "! @enum: [ "female", "male", "undefined" ]
      gender type string,
      "! address of the person
      begin of address,
        "! the street
        street     type string,
        "! @deprecated
        old_street type string,
      end of address,
    end of t_person,
    "! list of people
    t_person_list type standard table of t_person with empty key.

  "! returns a list of people
  methods list
    returning value(person_list) type t_person_list.

endinterface.
