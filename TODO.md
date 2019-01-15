# can-dbc
- [ ] Handle escaped quotes in `char_string` parser e.g. `"abc\"cdef\""`
- [ ] Check whether the STRING is valid in `BA_DEF_  "SAE_J1939_75_SpecVersion" STRING;` / `BA_DEF_ SG_  "GenSigEVName" STRING;`
- [ ] Better error messages for failed parsers
- [ ] Handle missing and multi trailing space in DBC file
- [ ] Handle possible spaces before commas

# dbcc
- [ ] Handle incomplete parsing by partially generating file to the point where the parser failed
- [ ] Handle invalid c_string values and other values that can not easily be turned into ENUM names when generating ENUMS examples:
  - `VAL_ 2566722302 ReverseCurrentRangeSetting 31 "NotAvailable" 30 "Error" 16 "11101NotUsed" 15 "16" 14 "15" 13 "14" 12 "13" 11 "12" 10 "11" 9 "10" 8 "9" 7 "8" 6 "7" 5 "6" 4 "5" 3 "4" 2 "3" 1 "2" 0 "1";`
  - `VAL_ 2560032510 LwVltgDscnnctDsredOperatingMode 15 "NoChange" 6 "1110Reserved" 5 "ManualConnect" 4 "ManualDisconnect" 1 "0011Reserved" 0 "Automatic";`