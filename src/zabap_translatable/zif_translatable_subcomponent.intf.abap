INTERFACE zif_translatable_subcomponent PUBLIC.
  INTERFACES:
    zif_translatable.
  METHODS:
    modify_text IMPORTING new_text TYPE zif_translatable=>t_text.
  DATA:
    sub_type       TYPE string READ-ONLY.
ENDINTERFACE.
