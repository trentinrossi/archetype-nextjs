# COCRDSLC.cbl: Credit Card Detail View

## Overview
This program, COCRDSLC.cbl, is part of the CardDemo application and serves as the business logic layer for viewing credit card details. It handles the process of accepting and processing credit card detail requests from users. The program interacts with a user interface to gather input, validates the input, retrieves the requested credit card information from a database, and displays the results back to the user.

<!-- general-rule-start -->
## General Business Rules:
1. The program allows users to view credit card details by entering either an account number, a card number, or both.
2. Input validation is performed to ensure that the account number (if provided) is a valid 11-digit number and the card number (if provided) is a valid 16-digit number.
3. The program can be accessed from the main menu or from a credit card list screen.
4. If coming from the credit card list screen, the selection criteria are pre-validated.
5. The program reads card information from a file named CARDDAT using the card number as the primary key.
6. An alternate index (CARDAIX) is available to access card information using the account number.
7. The program displays card details such as the embossed name, expiration date, and card status.
8. Error messages are displayed for invalid inputs or when requested information is not found.
9. The program supports navigation back to the main menu or the calling program using PF3.
10. The screen layout and attributes are dynamically adjusted based on the context (e.g., coming from the list screen vs. direct access).
<!-- general-rule-end -->

The program follows a structured approach with distinct sections for initialization, input processing, data retrieval, and output display. It uses CICS commands for screen handling and file access, demonstrating integration with the CICS transaction processing system.

The code includes error handling mechanisms, input validation, and user-friendly messages to guide the user through the process of viewing credit card details. It also incorporates security measures by validating user input and handling potential system errors gracefully.
## Dependencies

This program relies on several dependencies to function properly within the CardDemo application ecosystem. These dependencies include CICS commands for transaction processing, copybooks for shared data structures, and file systems for data storage and retrieval. Understanding these dependencies is crucial for any modernization efforts, as they represent integration points and potential areas for updating or replacing components.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| CICS | Customer Information Control System, used for transaction processing and screen handling | System | N/A | High - May need to be replaced or adapted in a modernized system |
| CARDDAT | Main card data file | File | LIT-CARDFILENAME | High - Data storage may need to be migrated to a modern database |
| CARDAIX | Alternate index for card data, accessed by account ID | File | LIT-CARDFILENAME-ACCT-PATH | High - May need to be replaced with database indexing |
| CVCRD01Y | Copybook for card-related data structures | Copybook | COPY CVCRD01Y | Medium - Data structures may need to be adapted |
| COCOM01Y | Copybook for application common area | Copybook | COPY COCOM01Y | Medium - Shared data structures may need redesign |
| DFHBMSCA | CICS BMS screen attribute definitions | Copybook | COPY DFHBMSCA | High - Screen handling may need modernization |
| DFHAID | CICS AID key definitions | Copybook | COPY DFHAID | Medium - Input handling may need updating |
| COTTL01Y | Copybook for screen titles | Copybook | COPY COTTL01Y | Low - May be replaced with modern UI frameworks |
| COCRDSL | Copybook for credit card search screen layout | Copybook | COPY COCRDSL | High - Screen layout may need redesign for web or mobile |
| CSDAT01Y | Copybook for current date handling | Copybook | COPY CSDAT01Y | Low - Date handling may be updated |
| CSMSG01Y | Copybook for common messages | Copybook | COPY CSMSG01Y | Medium - Message handling may be centralized |
| CSMSG02Y | Copybook for abend variables | Copybook | COPY CSMSG02Y | Medium - Error handling may be redesigned |
| CSUSR01Y | Copybook for signed-on user data | Copybook | COPY CSUSR01Y | Medium - User authentication may be updated |
| CVACT02Y | Copybook for card record layout | Copybook | COPY CVACT02Y | High - Data structure may need adaptation for new storage |
| CVCUS01Y | Copybook for customer layout | Copybook | COPY CVCUS01Y | High - Customer data structure may need redesign |
| CSSTRPFY | Copybook for storing PF key | Copybook | COPY 'CSSTRPFY' | Medium - Input handling may be updated |
## Detailed Rules
## Data Structure

This section describes the key data structures used in the COCRDSLC program. These structures are crucial for handling credit card details and user interactions.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| 1 | WS-CICS-PROCESSNG-VARS | Group | N/A | CICS processing variables | Yes | N/A | Contains response codes and transaction ID | High | Control |
| 1.1 | WS-RESP-CD | Numeric | S9(09) COMP | CICS response code | Yes | ZEROS | Used for error handling | High | Control |
| 1.2 | WS-REAS-CD | Numeric | S9(09) COMP | CICS reason code | Yes | ZEROS | Used for detailed error information | High | Control |
| 1.3 | WS-TRANID | Alphanumeric | X(4) | Transaction ID | Yes | SPACES | Stores the current transaction ID | Medium | Control |
| 2 | WS-INPUT-FLAG | Alphanumeric | X(1) | Input validation flag | Yes | LOW-VALUES | 88 levels: INPUT-OK, INPUT-ERROR, INPUT-PENDING | High | Control |
| 3 | WS-EDIT-ACCT-FLAG | Alphanumeric | X(1) | Account filter validation flag | Yes | N/A | 88 levels for different validation states | High | Control |
| 4 | WS-EDIT-CARD-FLAG | Alphanumeric | X(1) | Card filter validation flag | Yes | N/A | 88 levels for different validation states | High | Control |
| 5 | WS-RETURN-FLAG | Alphanumeric | X(1) | Return flag | Yes | LOW-VALUES | 88 levels: WS-RETURN-FLAG-OFF, WS-RETURN-FLAG-ON | Medium | Control |
| 6 | WS-PFK-FLAG | Alphanumeric | X(1) | PF key validation flag | Yes | N/A | 88 levels: PFK-VALID, PFK-INVALID | Medium | Control |
| 7 | CARD-ACCT-ID-X | Alphanumeric | X(11) | Account ID | Yes | N/A | Redefined as numeric CARD-ACCT-ID-N | High | Business |
| 8 | CARD-CVV-CD-X | Alphanumeric | X(03) | Card CVV code | Yes | N/A | Redefined as numeric CARD-CVV-CD-N | High | Business |
| 9 | CARD-CARD-NUM-X | Alphanumeric | X(16) | Card number | Yes | N/A | Redefined as numeric CARD-CARD-NUM-N | High | Business |
| 10 | CARD-NAME-EMBOSSED-X | Alphanumeric | X(50) | Name embossed on card | Yes | N/A | N/A | High | Business |
| 11 | CARD-STATUS-X | Alphanumeric | X | Card status | Yes | N/A | N/A | High | Business |
| 12 | CARD-EXPIRAION-DATE-X | Alphanumeric | X(10) | Card expiration date | Yes | N/A | Redefined with separate year, month, day fields | High | Business |
| 13 | WS-CARD-RID | Group | N/A | Card record ID structure | Yes | N/A | Used for file access | High | Control |
| 13.1 | WS-CARD-RID-CARDNUM | Alphanumeric | X(16) | Card number for record access | Yes | N/A | Primary key for CARDDAT file | High | Control |
| 13.2 | WS-CARD-RID-ACCT-ID | Numeric | 9(11) | Account ID for record access | Yes | N/A | Alternate key for CARDAIX file | High | Control |
| 14 | WS-FILE-ERROR-MESSAGE | Group | N/A | File error message structure | No | N/A | Used for constructing error messages | Medium | Control |
| 15 | WS-LONG-MSG | Alphanumeric | X(500) | Long message for errors | No | N/A | Used for detailed error reporting | Medium | Control |
| 16 | WS-INFO-MSG | Alphanumeric | X(40) | Informational message | No | N/A | Various 88 levels for different messages | High | Control |
| 17 | WS-RETURN-MSG | Alphanumeric | X(75) | Return message | No | SPACES | Various 88 levels for different return messages | High | Control |
| 18 | CARDDEMO-COMMAREA | Group | N/A | Common area for program communication | Yes | N/A | Defined in COCOM01Y copybook | High | Control |
| 19 | WS-THIS-PROGCOMMAREA | Group | N/A | Program-specific common area | Yes | N/A | Contains program context information | High | Control |
| 20 | CCRDSLAO | Group | N/A | Output map area | Yes | N/A | Defined in COCRDSL copybook | High | Presentation |
| 21 | CCRDSLAI | Group | N/A | Input map area | Yes | N/A | Defined in COCRDSL copybook | High | Presentation |



<!--rule-start-->
## Reviewed Rule 1
- ## Rule / Function / Method - Main Program Flow:
   - Detailed Description of the Rule:
     This rule defines the main program flow for the COCRDSLC program, which handles credit card detail viewing.
   - What it proposes to do:
     Control the overall execution of the program, including initialization, input processing, data retrieval, and output display.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Initialize the program:
      - Set up error handling using EXEC CICS HANDLE ABEND.
      - Initialize working storage variables.
      - Store the transaction ID (CCDL) in WS-TRANID.
      - Clear the return message (WS-RETURN-MSG-OFF).
   2. Check if there's incoming data (EIBCALEN > 0):
      - If yes, move the incoming data to CARDDEMO-COMMAREA and WS-THIS-PROGCOMMAREA.
      - If no, initialize CARDDEMO-COMMAREA and WS-THIS-PROGCOMMAREA.
   3. Perform YYYY-STORE-PFKEY to handle function key input.
   4. Validate the AID (Attention Identifier):
      - Set PFK-VALID if CCARD-AID-ENTER or CCARD-AID-PFK03.
      - If invalid, set CCARD-AID-ENTER.
   5. Process based on the AID:
      - If PFK03 (Exit):
        - Determine the program and transaction to return to (menu or calling program).
        - Perform EXEC CICS XCTL to the determined program.
      - If coming from the credit card list screen (CDEMO-FROM-PROGRAM = LIT-CCLISTPGM):
        - Set INPUT-OK.
        - Move CDEMO-ACCT-ID to CC-ACCT-ID-N and CDEMO-CARD-NUM to CC-CARD-NUM-N.
        - Perform 9000-READ-DATA.
        - Perform 1000-SEND-MAP.
      - If entering the program for the first time:
        - Perform 1000-SEND-MAP.
      - If re-entering the program:
        - Perform 2000-PROCESS-INPUTS.
        - If INPUT-ERROR, perform 1000-SEND-MAP.
        - Otherwise, perform 9000-READ-DATA and 1000-SEND-MAP.
   6. If there are any errors, display the error message and return.
   7. Perform EXEC CICS RETURN with the updated commarea.
<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
```markdown
- ## Rule / Function / Method - Send Map (1000-SEND-MAP):
   - Detailed Description of the Rule:
     This rule handles the preparation and sending of the screen map to display credit card details.
   - What it proposes to do:
     Set up the screen layout, populate fields with data, and send the map to the user's terminal.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Perform 1100-SCREEN-INIT:
      - Move LOW-VALUES to CCRDSLAO (clear the output area).
      - Set up screen titles, transaction name, and program name.
      - Get the current date and time and format them for display.
   2. Perform 1200-SETUP-SCREEN-VARS:
      - If EIBCALEN = 0 or coming from the menu program and not re-entering:
        - Initialize CARDDEMO-COMMAREA and WS-THIS-PROGCOMMAREA.
        - Set WS-PROMPT-FOR-INPUT.
      - Otherwise:
        - Move DFHCOMMAREA to CARDDEMO-COMMAREA and WS-THIS-PROGCOMMAREA.
      - If coming from the credit card list screen (COCRDLIC):
        - Move CDEMO-ACCT-ID to ACCTSIDO of CCRDSLAO.
        - Move CDEMO-CARD-NUM to CARDSIDO of CCRDSLAO.
      - If FOUND-CARDS-FOR-ACCOUNT:
        - Move card details (name, expiration date, status) to their respective fields.
      - Set up info and error messages.
   3. Perform 1300-SETUP-SCREEN-ATTRS:
      - Set protection flags for input fields based on context:
        - If coming from credit card list screen, protect ACCTSIDA and CARDSIDA.
        - Otherwise, set them as unprotected.
      - Position the cursor based on input validation results.
      - Set up color attributes for fields:
        - If coming from credit card list screen, set ACCTSIDC and CARDSIDC to default color.
        - Set ACCTSIDC to red if FLG-ACCTFILTER-NOT-OK.
        - Set CARDSIDC to red if FLG-CARDFILTER-NOT-OK.
        - If re-entering and fields are blank, set to red and display '*'.
   4. Perform 1400-SEND-SCREEN:
      - Set CCARD-NEXT-MAPSET to LIT-THISMAPSET and CCARD-NEXT-MAP to LIT-THISMAP.
      - Set CDEMO-PGM-REENTER to TRUE.
      - Execute EXEC CICS SEND MAP to display the screen with cursor positioning, erase the screen, and free the keyboard.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Process Inputs (2000-PROCESS-INPUTS):
   - Detailed Description of the Rule:
     This rule handles the processing of user inputs from the credit card detail screen.
   - What it proposes to do:
     Receive and validate user inputs, preparing them for further processing.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Perform 2100-RECEIVE-MAP:
      - Execute EXEC CICS RECEIVE MAP to get user inputs.
   2. Perform 2200-EDIT-MAP-INPUTS:
      - Set INPUT-OK, FLG-CARDFILTER-ISVALID, and FLG-ACCTFILTER-ISVALID to TRUE.
      - Replace '*' or spaces in ACCTSIDI and CARDSIDI with LOW-VALUES.
      - Perform 2210-EDIT-ACCOUNT:
        - Validate the account number input:
          - If blank, set INPUT-ERROR and FLG-ACCTFILTER-BLANK.
          - If not numeric or not 11 digits, set INPUT-ERROR and FLG-ACCTFILTER-NOT-OK.
          - If valid, move CC-ACCT-ID to CDEMO-ACCT-ID and set FLG-ACCTFILTER-ISVALID.
      - Perform 2220-EDIT-CARD:
        - Validate the card number input:
          - If blank, set INPUT-ERROR and FLG-CARDFILTER-BLANK.
          - If not numeric or not 16 digits, set INPUT-ERROR and FLG-CARDFILTER-NOT-OK.
          - If valid, move CC-CARD-NUM-N to CDEMO-CARD-NUM and set FLG-CARDFILTER-ISVALID.
      - If both account and card filters are blank, set NO-SEARCH-CRITERIA-RECEIVED.
   3. Move WS-RETURN-MSG to CCARD-ERROR-MSG.
   4. Set up CCARD-NEXT-PROG to LIT-THISPGM, CCARD-NEXT-MAPSET to LIT-THISMAPSET, and CCARD-NEXT-MAP to LIT-THISMAP.
   5. If INPUT-ERROR is set:
      - Move WS-RETURN-MSG to CCARD-ERROR-MSG.
      - Perform 1000-SEND-MAP.
      - Go to COMMON-RETURN.
   6. If no errors:
      - Perform 9000-READ-DATA.
      - Perform 1000-SEND-MAP.
      - Go to COMMON-RETURN.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
```markdown
- ## Rule / Function / Method - Read Data (9000-READ-DATA):
   - Detailed Description of the Rule:
     This rule handles the retrieval of credit card data from the file system.
   - What it proposes to do:
     Read the card details based on the provided card number.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Perform 9100-GETCARD-BYACCTCARD:
      - Move CC-CARD-NUM to WS-CARD-RID-CARDNUM.
      - Execute EXEC CICS READ for the CARDDAT file using WS-CARD-RID-CARDNUM as the key.
      - Evaluate the response:
        - If NORMAL, set FOUND-CARDS-FOR-ACCOUNT to TRUE.
        - If NOTFND, set INPUT-ERROR, FLG-ACCTFILTER-NOT-OK, FLG-CARDFILTER-NOT-OK, and DID-NOT-FIND-ACCTCARD-COMBO to TRUE.
        - For other responses, set INPUT-ERROR to TRUE, FLG-ACCTFILTER-NOT-OK to TRUE, and populate error message details (including operation name, file name, response code, and reason code).
   2. 9150-GETCARD-BYACCT (not directly called in this flow):
      - Execute EXEC CICS READ for the CARDAIX file using WS-CARD-RID-ACCT-ID as the key.
      - Evaluate the response:
        - If NORMAL, set FOUND-CARDS-FOR-ACCOUNT to TRUE.
        - If NOTFND, set INPUT-ERROR, FLG-ACCTFILTER-NOT-OK, and DID-NOT-FIND-ACCT-IN-CARDXREF to TRUE.
        - For other responses, set INPUT-ERROR, FLG-ACCTFILTER-NOT-OK to TRUE, and populate error message details (including operation name, file name, response code, and reason code).
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
```markdown
- ## Rule / Function / Method - Error Handling (ABEND-ROUTINE):
   - Detailed Description of the Rule:
     This rule handles unexpected abends (abnormal ends) in the program.
   - What it proposes to do:
     Provide a graceful error message and terminate the program in case of unexpected errors.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. If ABEND-MSG is LOW-VALUES, set it to 'UNEXPECTED ABEND OCCURRED.'
   2. Move LIT-THISPGM to ABEND-CULPRIT.
   3. Execute EXEC CICS SEND to display the ABEND-DATA.
   4. Execute EXEC CICS HANDLE ABEND CANCEL to reset the abend handling.
   5. Execute EXEC CICS ABEND with ABCODE('9999') to terminate the program.
```

<!--rule-end-->