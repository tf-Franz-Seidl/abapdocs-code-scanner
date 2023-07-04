"! is used to demonstrate the usage of abapDocs Code Scanner<br/>
"! the type declaration and the method are used in example code pieces<br/>
"! have a look to the unit test of zcl_tf_abapdocs_code_scanner
interface zif_acs_abapdocs_example
  public .

  types:
    "! address base to show include
    begin of t_address_base,
      "! @maximum: 99999
      postcode type int4,
      "! city or village
      city     type string,
    end of t_address_base,

    "! data of a person
    begin of t_person,
      "! name of the person
      name   type string,
      "! the gender
      "! @enum: [ "female",
      "!          "male",
      "!          "undefined" ]
      gender type string,
      "! address of the person
      begin of address,
        "! the street
        street     type string,
        "! @deprecated
        old_street type string.

        include type t_address_base.
        include type zcl_acs_abapdocs_annotation=>t_key_value.
      types: end of address,
    end of t_person,
    "! list of people
    t_person_list type standard table of t_person with empty key.


  "! returns a list of people
  "! @parameter gender | filter on gender
  "! @parameter street | filter on street
  "! @parameter person_list | list of found persons
  methods list
    importing gender             type string optional
              street             type string optional
    returning value(person_list) type t_person_list.

endinterface.
