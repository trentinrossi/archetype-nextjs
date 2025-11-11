# COACTVWC.cbl: Account View Request Processing

## Overview
This COBOL program, COACTVWC, is responsible for handling and processing account view requests in a credit card management system. It serves as the business logic layer for displaying account details to users. The program interacts with various data files to retrieve and display account, customer, and card information based on user input.

<!-- general-rule-start -->
## General Business Rules:
1. The program accepts an account number as input from the user.
2. It validates the input account number for format and existence in the system.
3. If the account number is valid, the program retrieves associated account details from the account master file.
4. It also fetches related customer information from the customer master file.
5. The program displays the retrieved account and customer details on the screen.
6. If any errors occur during the process (e.g., invalid input, account not found), appropriate error messages are displayed to the user.
7. The program supports navigation using PF keys, including the ability to return to the main menu.
8. It handles various scenarios such as initial entry, re-entry with input, and processing of retrieved data.
9. The program ensures data security by only displaying information for valid account numbers.
10. It formats and presents the data in a user-friendly manner, including proper formatting of dates, currency amounts, and sensitive information like SSN.
<!-- general-rule-end -->

Key functionalities include:
- Input validation for the account number
- Retrieval of account information from the account master file
- Fetching associated customer details from the customer master file
- Displaying formatted account and customer information
- Error handling and user-friendly message display
- Integration with the overall credit card management system through common areas and linkage sections

The program uses various CICS commands for screen handling, file operations, and program flow control. It also incorporates error handling and abend routines to ensure robust operation in a production environment.
## Dependencies

This program relies on several external dependencies for its functionality, including copybooks for data structures, file definitions, and common routines. These dependencies are crucial for the program's operation and would need to be considered in any modernization effort.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| COACTVW | BMS Map Copybook | Copybook | COACTVW.cpy | Contains screen layout definitions. May need to be replaced with a modern UI framework. |
| COCOM01Y | Application Commarea Copybook | Copybook | COCOM01Y.cpy | Defines common communication area. Important for inter-program communication. |
| CVCRD01Y | Credit Card Work Areas | Copybook | CVCRD01Y.cpy | Contains credit card related work areas. May need to be adapted for modern data structures. |
| COTTL01Y | Screen Titles | Copybook | COTTL01Y.cpy | Defines screen titles. Could be replaced with a configuration file in a modernized system. |
| CSDAT01Y | Current Date | Copybook | CSDAT01Y.cpy | Date handling routines. May be replaced with modern date libraries. |
| CSMSG01Y | Common Messages | Copybook | CSMSG01Y.cpy | Common message definitions. Could be moved to a centralized message management system. |
| CSMSG02Y | Abend Variables | Copybook | CSMSG02Y.cpy | Abend handling variables. Error handling might be redesigned in a modern system. |
| CSUSR01Y | Signed on User Data | Copybook | CSUSR01Y.cpy | User authentication data. May need to be integrated with a modern authentication system. |
| CVACT01Y | Account Record Layout | Copybook | CVACT01Y.cpy | Account data structure. Important for data migration and API design. |
| CVACT02Y | Customer Record Layout | Copybook | CVACT02Y.cpy | Customer data structure. Important for data migration and API design. |
| CVACT03Y | Card XREF Layout | Copybook | CVACT03Y.cpy | Card cross-reference data. May need to be redesigned in a relational database. |
| CVCUS01Y | Customer Layout | Copybook | CVCUS01Y.cpy | Customer data layout. Important for data migration and API design. |
| CSSTRPFY | PFKey Storage | Copybook | CSSTRPFY.cpy | Function key handling. May be replaced with modern UI event handling. |
| ACCTDAT | Account Master File | File | N/A | Critical data store. May need to be migrated to a modern database system. |
| CARDDAT | Card Master File | File | N/A | Critical data store. May need to be migrated to a modern database system. |
| CUSTDAT | Customer Master File | File | N/A | Critical data store. May need to be migrated to a modern database system. |
| CARDAIX | Card File Alternate Index | File | N/A | Index file. May be replaced by database indexing in a modern system. |
| CXACAIX | Card XREF Alternate Index | File | N/A | Index file. May be replaced by database indexing in a modern system. |

These dependencies are integral to the program's functionality, handling aspects such as data structures, file access, screen layouts, and common routines. In a modernization effort, each would need to be carefully evaluated and potentially replaced or redesigned to fit within a more modern architecture while maintaining the core business logic and data integrity.
## Detailed Rules
## Data Structure

This section describes the key data structures used in the COACTVWC program, focusing on the main fields used for account and customer information display.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| 1 | ACCTSIDI | Numeric | 11 | Account ID input field | Yes | None | Used for account lookup | Yes | Input |
| 2 | ACCTSIDO | Numeric | 11 | Account ID display field | Yes | None | Displays retrieved account number | Yes | Output |
| 3 | ACSTTUSO | Alphanumeric | 1 | Account Status | Yes | None | Displays account active status | Yes | Output |
| 4 | ACURBALO | Numeric | 15,2 | Current Balance | Yes | None | Displays current account balance | Yes | Output |
| 5 | ACRDLIMO | Numeric | 15,2 | Credit Limit | Yes | None | Displays account credit limit | Yes | Output |
| 6 | ACSHLIMO | Numeric | 15,2 | Cash Credit Limit | Yes | None | Displays cash credit limit | Yes | Output |
| 7 | ACRCYCRO | Numeric | 15,2 | Current Cycle Credit | Yes | None | Displays current cycle credit amount | Yes | Output |
| 8 | ACRCYDBO | Numeric | 15,2 | Current Cycle Debit | Yes | None | Displays current cycle debit amount | Yes | Output |
| 9 | ADTOPENO | Date | 10 | Account Open Date | Yes | None | Displays account opening date | Yes | Output |
| 10 | AEXPDTO | Date | 10 | Account Expiration Date | Yes | None | Displays account expiration date | Yes | Output |
| 11 | AREISDTO | Date | 10 | Account Reissue Date | Yes | None | Displays account reissue date | Yes | Output |
| 12 | AADDGRPO | Alphanumeric | 10 | Account Group ID | No | None | Displays account group identifier | Yes | Output |
| 13 | ACSTNUMO | Numeric | 9 | Customer Number | Yes | None | Displays associated customer number | Yes | Output |
| 14 | ACSTSSNO | Alphanumeric | 11 | Customer SSN | Yes | None | Displays formatted SSN (XXX-XX-XXXX) | Yes | Output |
| 15 | ACSTFCOO | Numeric | 3 | FICO Credit Score | No | None | Displays customer's FICO score | Yes | Output |
| 16 | ACSTDOBO | Date | 10 | Customer Date of Birth | Yes | None | Displays customer's date of birth | Yes | Output |
| 17 | ACSFNAMO | Alphanumeric | 25 | Customer First Name | Yes | None | Displays customer's first name | Yes | Output |
| 18 | ACSMNAMO | Alphanumeric | 25 | Customer Middle Name | No | None | Displays customer's middle name | Yes | Output |
| 19 | ACSLNAMO | Alphanumeric | 25 | Customer Last Name | Yes | None | Displays customer's last name | Yes | Output |
| 20 | ACSADL1O | Alphanumeric | 50 | Address Line 1 | Yes | None | Displays first line of customer's address | Yes | Output |
| 21 | ACSADL2O | Alphanumeric | 50 | Address Line 2 | No | None | Displays second line of customer's address | Yes | Output |
| 22 | ACSCITYO | Alphanumeric | 50 | City | Yes | None | Displays customer's city | Yes | Output |
| 23 | ACSSTTEO | Alphanumeric | 2 | State Code | Yes | None | Displays customer's state code | Yes | Output |
| 24 | ACSZIPCO | Alphanumeric | 5 | ZIP Code | Yes | None | Displays customer's ZIP code | Yes | Output |
| 25 | ACSCTRYO | Alphanumeric | 3 | Country Code | Yes | None | Displays customer's country code | Yes | Output |
| 26 | ACSPHN1O | Alphanumeric | 13 | Phone Number 1 | Yes | None | Displays customer's primary phone number | Yes | Output |
| 27 | ACSPHN2O | Alphanumeric | 13 | Phone Number 2 | No | None | Displays customer's secondary phone number | Yes | Output |
| 28 | ACSGOVTO | Alphanumeric | 20 | Government ID | No | None | Displays customer's government-issued ID | Yes | Output |
| 29 | ACSEFTCO | Alphanumeric | 10 | EFT Account ID | No | None | Displays customer's EFT account ID | Yes | Output |
| 30 | ACSPFLGO | Alphanumeric | 1 | Primary Card Holder Indicator | Yes | None | Indicates if customer is primary card holder | Yes | Output |
| 31 | ERRMSGO | Alphanumeric | 78 | Error Message | No | None | Displays error messages to user | Yes | Output |
| 32 | INFOMSGO | Alphanumeric | 45 | Information Message | No | None | Displays informational messages to user | Yes | Output |

This data structure represents the key fields used in the account view screen. It includes both input fields (like the account ID for lookup) and output fields that display various account and customer details. The structure is relevant for modernization as it defines the core data elements that would need to be maintained in any updated version of the system, though the presentation layer might change significantly in a modern application.



<!--rule-start-->
## Reviewed Rule 1
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Initialize Screen (1100-SCREEN-INIT)
   - Detailed Description of the Rule:
     This rule initializes the screen for displaying account information. It sets up the basic screen layout, including titles, transaction name, program name, current date, and time.
   - What it proposes to do:
     Prepare the screen with standard information before populating it with account-specific data.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Clear the screen output area (CACTVWAO) by setting it to LOW-VALUES.
   2. Get the current date and time using FUNCTION CURRENT-DATE.
   3. Set the screen titles:
      - Move CCDA-TITLE01 to TITLE01O of CACTVWAO
      - Move CCDA-TITLE02 to TITLE02O of CACTVWAO
   4. Set the transaction name (TRNNAMEO) to LIT-THISTRANID ('CAVW').
   5. Set the program name (PGMNAMEO) to LIT-THISPGM ('COACTVWC').
   6. Format the current date:
      - Extract month, day, and year from WS-CURDATE-DATA
      - Format as MM/DD/YY and move to CURDATEO of CACTVWAO
   7. Format the current time:
      - Extract hours, minutes, and seconds from WS-CURTIME-DATA
      - Format as HH:MM:SS and move to CURTIMEO of CACTVWAO
   8. Move FUNCTION CURRENT-DATE to WS-CURDATE-DATA.
   9. Set WS-CURDATE-MM to WS-CURDATE-MONTH.
   10. Set WS-CURDATE-DD to WS-CURDATE-DAY.
   11. Set WS-CURDATE-YY to the last two digits of WS-CURDATE-YEAR.
   12. Move WS-CURDATE-MM-DD-YY to CURDATEO of CACTVWAO.
   13. Set WS-CURTIME-HH to WS-CURTIME-HOURS.
   14. Set WS-CURTIME-MM to WS-CURTIME-MINUTE.
   15. Set WS-CURTIME-SS to WS-CURTIME-SECOND.
   16. Move WS-CURTIME-HH-MM-SS to CURTIMEO of CACTVWAO.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Setup Screen Variables (1200-SETUP-SCREEN-VARS)
   - Detailed Description of the Rule:
     This rule populates the screen with account and customer information if available. It handles both initial entry and re-entry scenarios.
   - What it proposes to do:
     Display account and customer details on the screen based on the data retrieved from files.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. If EIBCALEN (CICS Commarea length) is 0:
      - Set WS-PROMPT-FOR-INPUT to TRUE
   2. Else:
      - If FLG-ACCTFILTER-BLANK is set:
        - Move LOW-VALUES to ACCTSIDO of CACTVWAO
      - Else:
        - Move CC-ACCT-ID to ACCTSIDO of CACTVWAO
   3. If FOUND-ACCT-IN-MASTER or FOUND-CUST-IN-MASTER:
      - Move account details to screen fields:
        - ACCT-ACTIVE-STATUS to ACSTTUSO
        - ACCT-CURR-BAL to ACURBALO
        - ACCT-CREDIT-LIMIT to ACRDLIMO
        - ACCT-CASH-CREDIT-LIMIT to ACSHLIMO
        - ACCT-CURR-CYC-CREDIT to ACRCYCRO
        - ACCT-CURR-CYC-DEBIT to ACRCYDBO
        - ACCT-OPEN-DATE to ADTOPENO
        - ACCT-EXPIRAION-DATE to AEXPDTO
        - ACCT-REISSUE-DATE to AREISDTO
        - ACCT-GROUP-ID to AADDGRPO
   4. If FOUND-CUST-IN-MASTER:
      - Move customer details to screen fields:
        - CUST-ID to ACSTNUMO
        - Format CUST-SSN as XXX-XX-XXXX and move to ACSTSSNO
        - CUST-FICO-CREDIT-SCORE to ACSTFCOO
        - CUST-DOB-YYYY-MM-DD to ACSTDOBO
        - CUST-FIRST-NAME to ACSFNAMO
        - CUST-MIDDLE-NAME to ACSMNAMO
        - CUST-LAST-NAME to ACSLNAMO
        - CUST-ADDR-LINE-1 to ACSADL1O
        - CUST-ADDR-LINE-2 to ACSADL2O
        - CUST-ADDR-LINE-3 to ACSCITYO
        - CUST-ADDR-STATE-CD to ACSSTTEO
        - CUST-ADDR-ZIP to ACSZIPCO
        - CUST-ADDR-COUNTRY-CD to ACSCTRYO
        - CUST-PHONE-NUM-1 to ACSPHN1O
        - CUST-PHONE-NUM-2 to ACSPHN2O
        - CUST-GOVT-ISSUED-ID to ACSGOVTO
        - CUST-EFT-ACCOUNT-ID to ACSEFTCO
        - CUST-PRI-CARD-HOLDER-IND to ACSPFLGO
   5. If WS-NO-INFO-MESSAGE is set:
      - Set WS-PROMPT-FOR-INPUT to TRUE
   6. Move WS-RETURN-MSG to ERRMSGO of CACTVWAO
   7. Move WS-INFO-MSG to INFOMSGO of CACTVWAO
   8. If EIBCALEN is not 0 and WS-NO-INFO-MESSAGE is set:
      - Set WS-PROMPT-FOR-INPUT to TRUE
   9. Format CUST-SSN by concatenating:
      - CUST-SSN(1:3)
      - '-'
      - CUST-SSN(4:2)
      - '-'
      - CUST-SSN(6:4)
      Then move the result to ACSTSSNO of CACTVWAO
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
```markdown
- ## Rule / Function / Method - Setup Screen Attributes (1300-SETUP-SCREEN-ATTRS)
   - Detailed Description of the Rule:
     This rule sets up the visual attributes of the screen fields, such as protection status, cursor position, and color.
   - What it proposes to do:
     Enhance user interaction by highlighting important fields and positioning the cursor appropriately.

   ## Rule Status: 
   - Relevant for Modernization (though implementation may differ in modern UIs)

   ## Algorithm
   1. Set ACCTSIDA of CACTVWAI to DFHBMFSE (unprotected, alphanumeric)
   2. Position cursor based on field status:
      - If FLG-ACCTFILTER-NOT-OK or FLG-ACCTFILTER-BLANK:
        - Move -1 to ACCTSIDL of CACTVWAI (position cursor on account ID field)
      - Else:
        - Move -1 to ACCTSIDL of CACTVWAI (default cursor position)
   3. Set default color for ACCTSID field:
      - Move DFHDFCOL to ACCTSIDC of CACTVWAO
   4. If FLG-ACCTFILTER-NOT-OK:
      - Move DFHRED to ACCTSIDC of CACTVWAO (highlight in red)
   5. If FLG-ACCTFILTER-BLANK and CDEMO-PGM-REENTER:
      - Move '*' to ACCTSIDO of CACTVWAO
      - Move DFHRED to ACCTSIDC of CACTVWAO
   6. Set color for info message:
      - If WS-NO-INFO-MESSAGE:
        - Move DFHBMDAR to INFOMSGC of CACTVWAO (dark)
      - Else:
        - Move DFHNEUTR to INFOMSGC of CACTVWAO (neutral)
   7. Set protection status for all other fields:
      - Move DFHBMFSE to ACSTTUSA, ACURBALA, ACRDLIMA, ACSHLIMA, ACRCYCRA, ACRCYDBA, ADTOPENA, AEXPDTA, AREISDTA, AADDGRPA, ACSTNUMA, ACSTSSNA, ACSTFCOA, ACSTDOBA, ACSFNAMA, ACSMNAMA, ACSLNAMA, ACSADL1A, ACSADL2A, ACSCITYA, ACSSTTEA, ACSZIPCA, ACSCTRYA, ACSPHN1A, ACSPHN2A, ACSGOVTA, ACSEFTCA, ACSPFLGA of CACTVWAI (unprotected, alphanumeric)
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
```markdown
- ## Rule / Function / Method - Process Inputs (2000-PROCESS-INPUTS)
   - Detailed Description of the Rule:
     This rule handles the processing of user inputs, including receiving map data, editing inputs, and preparing for the next action.
   - What it proposes to do:
     Validate and process user inputs to ensure data integrity before further processing.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Perform 2100-RECEIVE-MAP to get user input
   2. Perform 2200-EDIT-MAP-INPUTS to validate the input:
      - Check if ACCTSIDI is '*' or spaces, if so set CC-ACCT-ID to low-values
      - Otherwise, move ACCTSIDI to CC-ACCT-ID
      - Perform 2210-EDIT-ACCOUNT:
        - If CC-ACCT-ID is low-values or spaces:
          - Set INPUT-ERROR and FLG-ACCTFILTER-BLANK
          - Set WS-PROMPT-FOR-ACCT if WS-RETURN-MSG-OFF
          - Move zeroes to CDEMO-ACCT-ID
        - If CC-ACCT-ID is not numeric or equal to zeroes:
          - Set INPUT-ERROR and FLG-ACCTFILTER-NOT-OK
          - Set error message if WS-RETURN-MSG-OFF
          - Move zero to CDEMO-ACCT-ID
        - Otherwise:
          - Move CC-ACCT-ID to CDEMO-ACCT-ID
          - Set FLG-ACCTFILTER-ISVALID
      - If FLG-ACCTFILTER-BLANK, set NO-SEARCH-CRITERIA-RECEIVED
   3. Move WS-RETURN-MSG to CCARD-ERROR-MSG
   4. Set up for next program execution:
      - Move LIT-THISPGM to CCARD-NEXT-PROG
      - Move LIT-THISMAPSET to CCARD-NEXT-MAPSET
      - Move LIT-THISMAP to CCARD-NEXT-MAP
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
```markdown
- ## Rule / Function / Method - Edit Account Input (2210-EDIT-ACCOUNT)
   - Detailed Description of the Rule:
     This rule validates the account number input by the user.
   - What it proposes to do:
     Ensure that the entered account number is valid and exists in the system.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Set FLG-ACCTFILTER-NOT-OK to TRUE (assume invalid initially)
   2. If CC-ACCT-ID is LOW-VALUES or SPACES:
      - Set INPUT-ERROR to TRUE
      - Set FLG-ACCTFILTER-BLANK to TRUE
      - If WS-RETURN-MSG-OFF, set WS-PROMPT-FOR-ACCT to TRUE
      - Move ZEROES to CDEMO-ACCT-ID
      - Go to 2210-EDIT-ACCOUNT-EXIT
   3. If CC-ACCT-ID is NOT NUMERIC or EQUAL to ZEROES:
      - Set INPUT-ERROR to TRUE
      - Set FLG-ACCTFILTER-NOT-OK to TRUE
      - If WS-RETURN-MSG-OFF, set error message:
        "Account Filter must be a non-zero 11 digit number"
      - Move ZERO to CDEMO-ACCT-ID
      - Go to 2210-EDIT-ACCOUNT-EXIT
   4. Else:
      - Move CC-ACCT-ID to CDEMO-ACCT-ID
      - Set FLG-ACCTFILTER-ISVALID to TRUE
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 6
```markdown
- ## Rule / Function / Method - Read Account Data (9000-READ-ACCT)
   - Detailed Description of the Rule:
     This rule retrieves account and customer data from various files based on the input account number.
   - What it proposes to do:
     Fetch all relevant information for the requested account from different data sources.

   ## Rule Status: 
   - Relevant for Modernization (though data access methods may change)

   ## Algorithm
   1. Set WS-NO-INFO-MESSAGE to TRUE
   2. Move CDEMO-ACCT-ID to WS-CARD-RID-ACCT-ID
   3. Perform 9200-GETCARDXREF-BYACCT to get card cross-reference data
      - Read from CXACAIX file using WS-CARD-RID-ACCT-ID as key
      - If successful, move XREF-CUST-ID to CDEMO-CUST-ID and XREF-CARD-NUM to CDEMO-CARD-NUM
      - If not found, set INPUT-ERROR and FLG-ACCTFILTER-NOT-OK to TRUE, display error message
      - For other errors, set INPUT-ERROR and FLG-ACCTFILTER-NOT-OK to TRUE, display file error message
   4. If FLG-ACCTFILTER-NOT-OK, go to 9000-READ-ACCT-EXIT
   5. Perform 9300-GETACCTDATA-BYACCT to get account data
      - Read from ACCTDAT file using WS-CARD-RID-ACCT-ID as key
      - If successful, set FOUND-ACCT-IN-MASTER to TRUE
      - If not found, set INPUT-ERROR and FLG-ACCTFILTER-NOT-OK to TRUE, display error message
      - For other errors, set INPUT-ERROR and FLG-ACCTFILTER-NOT-OK to TRUE, display file error message
   6. If FLG-ACCTFILTER-NOT-OK, go to 9000-READ-ACCT-EXIT
   7. Move CDEMO-CUST-ID to WS-CARD-RID-CUST-ID
   8. Perform 9400-GETCUSTDATA-BYCUST to get customer data
      - Read from CUSTDAT file using WS-CARD-RID-CUST-ID as key
      - If successful, set FOUND-CUST-IN-MASTER to TRUE
      - If not found, set INPUT-ERROR and FLG-CUSTFILTER-NOT-OK to TRUE, display error message
      - For other errors, set INPUT-ERROR and FLG-CUSTFILTER-NOT-OK to TRUE, display file error message
   9. If FLG-CUSTFILTER-NOT-OK, go to 9000-READ-ACCT-EXIT
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 7
```markdown
- ## Rule / Function / Method - Get Card Cross-Reference Data (9200-GETCARDXREF-BYACCT)
   - Detailed Description of the Rule:
     This rule retrieves card cross-reference data for the given account number.
   - What it proposes to do:
     Fetch card information associated with the account from the cross-reference file.

   ## Rule Status: 
   - Relevant for Modernization (though data access method may change)

   ## Algorithm
   1. Execute CICS READ command:
      - Dataset: LIT-CARDXREFNAME-ACCT-PATH
      - Key: WS-CARD-RID-ACCT-ID-X
      - Into: CARD-XREF-RECORD
   2. Evaluate WS-RESP-CD:
      - If DFHRESP(NORMAL):
        - Move XREF-CUST-ID to CDEMO-CUST-ID
        - Move XREF-CARD-NUM to CDEMO-CARD-NUM
      - If DFHRESP(NOTFND):
        - Set INPUT-ERROR to TRUE
        - Set FLG-ACCTFILTER-NOT-OK to TRUE
        - If WS-RETURN-MSG-OFF, set error message:
          "Account: [account number] not found in Cross ref file. Resp: [response] Reas: [reason]"
      - For other responses:
        - Set INPUT-ERROR and FLG-ACCTFILTER-NOT-OK to TRUE
        - Prepare file error message with operation ('READ'), file name (LIT-CARDXREFNAME-ACCT-PATH), and response codes
   3. If an error occurs (INPUT-ERROR is TRUE):
      - The error message will be moved to WS-RETURN-MSG
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 8
```markdown
- ## Rule / Function / Method - Get Account Data (9300-GETACCTDATA-BYACCT)
   - Detailed Description of the Rule:
     This rule retrieves account data for the given account number from the account master file.
   - What it proposes to do:
     Fetch detailed account information from the account master file.

   ## Rule Status: 
   - Relevant for Modernization (though data access method may change)

   ## Algorithm
   1. Execute CICS READ command:
      - Dataset: LIT-ACCTFILENAME
      - Key: WS-CARD-RID-ACCT-ID-X
      - Into: ACCOUNT-RECORD
      - Length: LENGTH OF ACCOUNT-RECORD
   2. Evaluate WS-RESP-CD:
      - If DFHRESP(NORMAL):
        - Set FOUND-ACCT-IN-MASTER to TRUE
      - If DFHRESP(NOTFND):
        - Set INPUT-ERROR and FLG-ACCTFILTER-NOT-OK to TRUE
        - If WS-RETURN-MSG-OFF, set error message:
          "Account: [account number] not found in Acct Master file. Resp: [response] Reas: [reason]"
      - For other responses:
        - Set INPUT-ERROR and FLG-ACCTFILTER-NOT-OK to TRUE
        - Prepare file error message with operation ('READ'), file name (LIT-ACCTFILENAME), and response codes
   3. If an error occurred (INPUT-ERROR is TRUE):
      - Move the error message to WS-RETURN-MSG
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 9
```markdown
- ## Rule / Function / Method - Get Customer Data (9400-GETCUSTDATA-BYCUST)
   - Detailed Description of the Rule:
     This rule retrieves customer data for the associated customer ID from the customer master file.
   - What it proposes to do:
     Fetch detailed customer information related to the account.

   ## Rule Status: 
   - Relevant for Modernization (though data access method may change)

   ## Algorithm
   1. Execute CICS READ command:
      - Dataset: LIT-CUSTFILENAME
      - Key: WS-CARD-RID-CUST-ID-X
      - Into: CUSTOMER-RECORD
      - Length: LENGTH OF CUSTOMER-RECORD
   2. Evaluate WS-RESP-CD:
      - If DFHRESP(NORMAL):
        - Set FOUND-CUST-IN-MASTER to TRUE
      - If DFHRESP(NOTFND):
        - Set INPUT-ERROR and FLG-CUSTFILTER-NOT-OK to TRUE
        - If WS-RETURN-MSG-OFF, set error message:
          "CustId: [customer ID] not found in customer master. Resp: [response] REAS: [reason]"
      - For other responses:
        - Set INPUT-ERROR and FLG-CUSTFILTER-NOT-OK to TRUE
        - Prepare file error message with operation ('READ'), file name (LIT-CUSTFILENAME), and response codes
   3. If an error occurs (INPUT-ERROR is TRUE):
      - Move the error message to WS-RETURN-MSG
```

<!--rule-end-->