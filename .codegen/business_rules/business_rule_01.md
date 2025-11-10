# COCRDLIC.cbl: Credit Card List Program

## Overview
This COBOL program, COCRDLIC, is part of a credit card management system. It provides functionality to list credit cards based on certain criteria. The program allows users to view a list of credit cards, either for all cards if no context is passed and the user is an admin, or only the cards associated with a specific account if the user is not an admin.

<!-- general-rule-start -->
## General Business Rules:
1. The program lists credit cards based on user permissions and input criteria.
2. It supports pagination, allowing users to navigate through multiple pages of credit card listings.
3. Users can filter the list by account ID and/or card number.
4. The program displays up to 7 credit card records per page.
5. For each credit card, the program shows the account number, card number, and card status.
6. Users can select individual cards for detailed view or update operations.
7. The program handles various error conditions and displays appropriate messages.
8. It integrates with other modules in the credit card management system, such as card detail view and card update functions.
<!-- general-rule-end -->

Key functionalities include:
1. Receiving and validating user inputs for account ID and card number filters.
2. Reading credit card records from a file (CARDDAT) based on the filter criteria.
3. Displaying a paginated list of credit cards with options to view details or update.
4. Handling navigation between pages (previous and next).
5. Processing user selections for viewing card details or updating card information.
6. Integrating with other programs in the system for seamless navigation.

The program uses various CICS commands for screen handling, file operations, and program control. It also implements error handling and validation to ensure data integrity and provide a user-friendly experience.
## Dependencies

This program relies on several dependencies for its functionality, including CICS commands, copybooks, and file systems. These dependencies are crucial for the program's operation and will need to be considered in any modernization efforts.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| CICS | Customer Information Control System, used for transaction processing | System | N/A | High - May need to be replaced or emulated in a modern environment |
| CARDDAT | Card data file | File | LIT-CARD-FILE | High - Data storage may need to be migrated to a modern database |
| CARDAIX | Card data file index | File | LIT-CARD-FILE-ACCT-PATH | High - Indexing may need to be redesigned in a new data storage system |
| CVCRD01Y | Copybook for card-related data structures | Copybook | COPY CVCRD01Y | Medium - Data structures may need to be adapted for new programming languages |
| COCOM01Y | Copybook for common area | Copybook | COPY COCOM01Y | Medium - Shared data structures may need to be redesigned |
| COTTL01Y | Copybook for screen titles | Copybook | COPY COTTL01Y | Low - UI elements may be redesigned in a new interface |
| COCRDLI | Copybook for credit card list screen layout | Copybook | COPY COCRDLI | Medium - Screen layout may need to be adapted to a new UI framework |
| CSDAT01Y | Copybook for current date | Copybook | COPY CSDAT01Y | Low - Date handling may be done differently in a modern system |
| CSMSG01Y | Copybook for common messages | Copybook | COPY CSMSG01Y | Low - Message handling may be redesigned |
| CSUSR01Y | Copybook for signed-on user data | Copybook | COPY CSUSR01Y | Medium - User authentication and data may need to be adapted |
| CVACT02Y | Copybook for card record layout | Copybook | COPY CVACT02Y | High - Core data structure that may need to be adapted to a new data model |
| CSSTRPFY | Copybook for storing PFKey | Copybook | COPY 'CSSTRPFY' | Low - Function key handling may be replaced in a new UI system |
## Detailed Rules
## Data Structure

This section describes the key data structures used in the COCRDLIC program. These structures are crucial for handling credit card information, user interface elements, and program control.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| 1 | WS-CARD-RID | Group Item | 27 | Card Record ID | Yes | N/A | Used for file access | High | Control |
| 1.1 | WS-CARD-RID-CARDNUM | Numeric | 16 | Card Number | Yes | Spaces | Part of record key | High | Business |
| 1.2 | WS-CARD-RID-ACCT-ID | Numeric | 11 | Account ID | Yes | Zeros | Part of record key | High | Business |
| 2 | CC-WORK-AREA | Group Item | Varies | Card processing work area | Yes | N/A | Defined in CVCRD01Y | High | Control |
| 2.1 | CC-ACCT-ID | Numeric | 11 | Account ID for processing | Yes | Zeros | User input field | High | Business |
| 2.2 | CC-CARD-NUM | Numeric | 16 | Card Number for processing | Yes | Zeros | User input field | High | Business |
| 3 | WS-SCREEN-DATA | Group Item | 196 | Screen data array | Yes | N/A | Holds displayed card data | High | UI |
| 3.1 | WS-SCREEN-ROWS | Group Item | 28 | Occurs 7 times | Yes | N/A | Individual row data | High | UI |
| 3.1.1 | WS-ROW-ACCTNO | Numeric | 11 | Account number for display | Yes | Spaces | Part of screen row | High | UI |
| 3.1.2 | WS-ROW-CARD-NUM | Numeric | 16 | Card number for display | Yes | Spaces | Part of screen row | High | UI |
| 3.1.3 | WS-ROW-CARD-STATUS | Alphanumeric | 1 | Card status for display | Yes | Spaces | Part of screen row | High | UI |
| 4 | WS-EDIT-SELECT-FLAGS | Alphanumeric | 7 | Selection flags for rows | Yes | Low-values | User input for row selection | High | Control |
| 5 | CARDDEMO-COMMAREA | Group Item | Varies | Common area for program communication | Yes | N/A | Defined in COCOM01Y | High | Control |
| 5.1 | CDEMO-ACCT-ID | Numeric | 11 | Account ID for processing | Yes | Zeros | Passed between programs | High | Business |
| 5.2 | CDEMO-CARD-NUM | Numeric | 16 | Card Number for processing | Yes | Zeros | Passed between programs | High | Business |
| 6 | WS-THIS-PROGCOMMAREA | Group Item | Varies | Program-specific common area | Yes | N/A | Holds program state | High | Control |
| 6.1 | WS-CA-LAST-CARDKEY | Group Item | 27 | Last card key displayed | Yes | N/A | For pagination | High | Control |
| 6.2 | WS-CA-FIRST-CARDKEY | Group Item | 27 | First card key displayed | Yes | N/A | For pagination | High | Control |
| 6.3 | WS-CA-SCREEN-NUM | Numeric | 1 | Current screen number | Yes | 0 | For pagination | Medium | Control |
| 7 | CCRDLIAI | Group Item | Varies | Input map area | Yes | N/A | Defined in COCRDLI | High | UI |
| 8 | CCRDLIAO | Group Item | Varies | Output map area | Yes | N/A | Defined in COCRDLI | High | UI |



<!--rule-start-->
## Reviewed Rule 1
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Initialize Program

- Detailed Description of the Rule:
  This rule initializes the program by setting up the working storage variables and handling the program's entry point.

- What it proposes to do:
  Prepare the program for execution by initializing variables and determining the program's context (first run or subsequent interaction).

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Initialize the CC-WORK-AREA, WS-MISC-STORAGE, and WS-COMMAREA to their default values.
2. Set WS-TRANID to 'CCLI' (the transaction ID for this program).
3. Clear any existing error messages by setting WS-ERROR-MSG-OFF to TRUE.
4. Check if EIBCALEN (the length of the passed data) is 0:
   a. If EIBCALEN is 0 (first run):
      - Initialize CARDDEMO-COMMAREA and WS-THIS-PROGCOMMAREA.
      - Set CDEMO-FROM-TRANID to 'CCLI'.
      - Set CDEMO-FROM-PROGRAM to 'COCRDLIC'.
      - Set CDEMO-USRTYP-USER to TRUE.
      - Set CDEMO-PGM-ENTER to TRUE.
      - Set CDEMO-LAST-MAP to 'CCRDLIA'.
      - Set CDEMO-LAST-MAPSET to 'COCRDLI'.
      - Set CA-FIRST-PAGE to TRUE.
      - Set CA-LAST-PAGE-NOT-SHOWN to TRUE.
   b. If EIBCALEN is not 0 (subsequent interaction):
      - Move the first part of DFHCOMMAREA to CARDDEMO-COMMAREA.
      - Move the second part of DFHCOMMAREA to WS-THIS-PROGCOMMAREA.
5. If coming from the menu program (CDEMO-PGM-ENTER is TRUE and CDEMO-FROM-PROGRAM is not 'COCRDLIC'):
   - Initialize WS-THIS-PROGCOMMAREA.
   - Set CDEMO-PGM-ENTER to TRUE.
   - Set CDEMO-LAST-MAP to 'CCRDLIA'.
   - Set CA-FIRST-PAGE to TRUE.
   - Set CA-LAST-PAGE-NOT-SHOWN to TRUE.
6. Perform the YYYY-STORE-PFKEY routine to handle function key mapping.
7. If EIBCALEN > 0 and CDEMO-FROM-PROGRAM equals 'COCRDLIC', perform the 2000-RECEIVE-MAP routine to read and edit the inputs given.
8. Check the mapped key to see if it's valid at this point:
   - Valid keys are: Enter, PF3 (Exit), PF7 (Page Up), PF8 (Page Down)
   - If the key is invalid, set it to Enter.
9. If PF3 is pressed and CDEMO-FROM-PROGRAM equals 'COCRDLIC':
   - Set up the return to the menu program (COMEN01C).
   - Set WS-EXIT-MESSAGE to TRUE.
   - Perform an EXEC CICS XCTL to the menu program.
10. If the user did not press PF8, reset the last page flag (set CA-LAST-PAGE-NOT-SHOWN to TRUE).
11. Based on the input and key pressed, perform one of the following actions:
    - Handle input errors
    - Handle PF7 (Page Up) when already on the first page
    - Handle PF3 (Exit) when coming from another program
    - Handle PF8 (Page Down) when more pages exist
    - Handle PF7 (Page Up) when not on the first page
    - Handle Enter key for viewing card details
    - Handle Enter key for updating card details
12. If there are no specific actions to take, read forward in the card file and display the results.
13. Set up the next program to be called (usually this same program, COCRDLIC).
14. Prepare the commarea for the next program call, including both CARDDEMO-COMMAREA and WS-THIS-PROGCOMMAREA.
15. Perform an EXEC CICS RETURN with the prepared commarea.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
Here's the revised business rule incorporating all the details from the COBOL program:

```markdown
- ## Rule / Function / Method - Receive Map

- Detailed Description of the Rule:
  This rule handles receiving user input from the screen map and performs initial validation.

- What it proposes to do:
  Capture user input from the screen, store it in the program's working storage, and perform initial validation of the input data.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Execute CICS RECEIVE MAP command for 'CCRDLIA' mapset.
2. Store the received data in the CCRDLIAI structure.
3. Move ACCTSIDI of CCRDLIAI to CC-ACCT-ID.
4. Move CARDSIDI of CCRDLIAI to CC-CARD-NUM.
5. Move CRDSEL1I through CRDSEL7I of CCRDLIAI to WS-EDIT-SELECT(1) through WS-EDIT-SELECT(7) respectively.
6. Perform initial validation:
   a. For CC-ACCT-ID:
      - If blank, low-values, or zeros, set FLG-ACCTFILTER-BLANK to TRUE and move zeros to CDEMO-ACCT-ID.
      - If not numeric or not 11 digits, set INPUT-ERROR, FLG-ACCTFILTER-NOT-OK, and FLG-PROTECT-SELECT-ROWS-YES to TRUE. Set error message and move zero to CDEMO-ACCT-ID.
      - If valid, move CC-ACCT-ID to CDEMO-ACCT-ID and set FLG-ACCTFILTER-ISVALID to TRUE.
   b. For CC-CARD-NUM:
      - If blank, low-values, or zeros, set FLG-CARDFILTER-BLANK to TRUE and move zeros to CDEMO-CARD-NUM.
      - If not numeric or not 16 digits, set INPUT-ERROR, FLG-CARDFILTER-NOT-OK, and FLG-PROTECT-SELECT-ROWS-YES to TRUE. Set error message and move zero to CDEMO-CARD-NUM.
      - If valid, move CC-CARD-NUM-N to CDEMO-CARD-NUM and set FLG-CARDFILTER-ISVALID to TRUE.
   c. For WS-EDIT-SELECT array:
      - Count occurrences of 'S' and 'U'.
      - If count > 1, set INPUT-ERROR and WS-MORE-THAN-1-ACTION to TRUE.
      - For each element (1 to 7):
        - If 'S' or 'U', set I-SELECTED to the index.
        - If blank or low-values, continue.
        - Otherwise, set INPUT-ERROR and WS-INVALID-ACTION-CODE to TRUE.
7. If any validation errors occur, set appropriate error flags and messages for display on the next screen.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Edit Inputs

- Detailed Description of the Rule:
  This rule validates the user inputs received from the screen and sets appropriate flags for further processing.

- What it proposes to do:
  Ensure that the account ID and card number inputs are valid and set appropriate flags for further processing.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Set INPUT-OK to TRUE.
2. Set FLG-PROTECT-SELECT-ROWS-NO to TRUE.
3. Perform Edit Account:
   a. If CC-ACCT-ID is blank, spaces, or zero:
      - Set FLG-ACCTFILTER-BLANK to TRUE.
      - Move ZEROES to CDEMO-ACCT-ID.
   b. If CC-ACCT-ID is not numeric or not 11 digits:
      - Set INPUT-ERROR to TRUE.
      - Set FLG-ACCTFILTER-NOT-OK to TRUE.
      - Set FLG-PROTECT-SELECT-ROWS-YES to TRUE.
      - Set error message to 'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER'.
      - Move ZERO to CDEMO-ACCT-ID.
   c. If CC-ACCT-ID is valid:
      - Move CC-ACCT-ID to CDEMO-ACCT-ID.
      - Set FLG-ACCTFILTER-ISVALID to TRUE.
4. Perform Edit Card:
   a. If CC-CARD-NUM is blank, spaces, or zero:
      - Set FLG-CARDFILTER-BLANK to TRUE.
      - Move ZEROES to CDEMO-CARD-NUM.
   b. If CC-CARD-NUM is not numeric or not 16 digits:
      - Set INPUT-ERROR to TRUE.
      - Set FLG-CARDFILTER-NOT-OK to TRUE.
      - Set FLG-PROTECT-SELECT-ROWS-YES to TRUE.
      - If WS-ERROR-MSG-OFF, set error message to 'CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER'.
      - Move ZERO to CDEMO-CARD-NUM.
   c. If CC-CARD-NUM is valid:
      - Move CC-CARD-NUM-N to CDEMO-CARD-NUM.
      - Set FLG-CARDFILTER-ISVALID to TRUE.
5. Perform Edit Array:
   a. If INPUT-ERROR, skip this step.
   b. Count the number of 'S' and 'U' selections in WS-EDIT-SELECT-FLAGS.
   c. If more than one selection is made:
      - Set INPUT-ERROR to TRUE.
      - Set WS-MORE-THAN-1-ACTION to TRUE.
      - Mark the selected rows in WS-EDIT-SELECT-ERROR-FLAGS with '1'.
   d. For each row (1 to 7):
      - If the selection is valid ('S' or 'U'), store the row number in I-SELECTED.
      - If the selection is blank, continue.
      - If the selection is invalid:
        * Set INPUT-ERROR to TRUE.
        * Mark the row as an error in WS-ROW-CRDSELECT-ERROR.
        * If WS-ERROR-MSG-OFF, set WS-INVALID-ACTION-CODE to TRUE.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Read Forward

- Detailed Description of the Rule:
  This rule reads credit card records forward from the CARDDAT file based on the current position and filter criteria.

- What it proposes to do:
  Retrieve and display the next set of credit card records that match the filter criteria.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Initialize WS-ALL-ROWS to low values.
2. Start browsing the CARDDAT file from the current position (WS-CARD-RID-CARDNUM).
3. Initialize WS-SCRN-COUNTER to zero.
4. Set CA-NEXT-PAGE-EXISTS to TRUE.
5. Set MORE-RECORDS-TO-READ to TRUE.
6. Perform until READ-LOOP-EXIT:
   a. Read the next record from CARDDAT file.
   b. If read is successful (NORMAL or DUPREC):
      - Perform 9500-FILTER-RECORDS to check if the record matches the filter criteria.
      - If the record should not be excluded:
        * Increment WS-SCRN-COUNTER.
        * Store the card details (number, account ID, status) in the corresponding WS-ROW fields.
        * If it's the first record:
          - Update WS-CA-FIRST-CARD-ACCT-ID and WS-CA-FIRST-CARD-NUM.
          - If WS-CA-SCREEN-NUM is 0, add 1 to it.
      - If WS-SCRN-COUNTER reaches WS-MAX-SCREEN-LINES (7):
        * Set READ-LOOP-EXIT to TRUE.
        * Update WS-CA-LAST-CARD-ACCT-ID and WS-CA-LAST-CARD-NUM.
        * Try to read one more record to determine if there are more pages:
          - If successful, set CA-NEXT-PAGE-EXISTS to TRUE and update WS-CA-LAST-CARD-ACCT-ID and WS-CA-LAST-CARD-NUM.
          - If ENDFILE, set CA-NEXT-PAGE-NOT-EXISTS to TRUE and set error message to 'NO MORE RECORDS TO SHOW'.
          - For any other response, set READ-LOOP-EXIT to TRUE and set appropriate error message with file operation details.
   c. If end of file is reached:
      - Set READ-LOOP-EXIT to TRUE.
      - Set CA-NEXT-PAGE-NOT-EXISTS to TRUE.
      - Update WS-CA-LAST-CARD-ACCT-ID and WS-CA-LAST-CARD-NUM.
      - Set error message to 'NO MORE RECORDS TO SHOW'.
      - If WS-CA-SCREEN-NUM is 1 and WS-SCRN-COUNTER is 0, set WS-NO-RECORDS-FOUND to TRUE.
   d. If any other error occurs:
      - Set READ-LOOP-EXIT to TRUE.
      - Set appropriate error message with file operation details.
7. End the browse operation on the CARDDAT file.

## Additional Details:
- The rule uses WS-CARD-RID-CARDNUM as the key for browsing and reading records.
- The maximum number of records displayed per page is 7 (WS-MAX-SCREEN-LINES).
- The rule handles both NORMAL and DUPREC responses when reading records.
- Error messages are set in WS-ERROR-MSG for various scenarios.
- The rule updates WS-CA-SCREEN-NUM, WS-CA-FIRST-CARD-ACCT-ID, WS-CA-FIRST-CARD-NUM, WS-CA-LAST-CARD-ACCT-ID, and WS-CA-LAST-CARD-NUM as it processes records.
- The 9500-FILTER-RECORDS subroutine is used to apply additional filtering based on account ID and card number if specified.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
Here's the revised business rule incorporating all the details from the COBOL program:

```markdown
- ## Rule / Function / Method - Read Backwards

- Detailed Description of the Rule:
  This rule reads credit card records backwards from the CARDDAT file based on the current position and filter criteria.

- What it proposes to do:
  Retrieve and display the previous set of credit card records that match the filter criteria.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Initialize WS-ALL-ROWS to low values.
2. Move WS-CA-FIRST-CARDKEY to WS-CA-LAST-CARDKEY.
3. Start browsing the CARDDAT file from the current position (WS-CARD-RID-CARDNUM).
4. Set WS-SCRN-COUNTER to WS-MAX-SCREEN-LINES + 1 (8).
5. Set CA-NEXT-PAGE-EXISTS to TRUE.
6. Set MORE-RECORDS-TO-READ to TRUE.
7. Read the previous record to position the cursor.
8. Perform until READ-LOOP-EXIT:
   a. Read the previous record from CARDDAT file.
   b. If read is successful:
      - Perform 9500-FILTER-RECORDS to check if the record matches the filter criteria:
        * If FLG-ACCTFILTER-ISVALID is set, check if CARD-ACCT-ID matches CC-ACCT-ID.
        * If FLG-CARDFILTER-ISVALID is set, check if CARD-NUM matches CC-CARD-NUM-N.
      - If the record should not be excluded:
        * Store the card details (number, account ID, status) in the corresponding WS-ROW fields.
        * Decrement WS-SCRN-COUNTER.
        * If WS-SCRN-COUNTER reaches 0:
          - Set READ-LOOP-EXIT to TRUE.
          - Update WS-CA-FIRST-CARD-ACCT-ID and WS-CA-FIRST-CARD-NUM with the current record's details.
   c. If DFHRESP(NORMAL) or DFHRESP(DUPREC) is not returned:
      - Set READ-LOOP-EXIT to TRUE.
      - Set appropriate error message with file operation details (READ, CARDDAT, response code, reason code).
9. End the browse operation on the CARDDAT file.
10. If no records are found and it's the first page (WS-CA-SCREEN-NUM = 1 and WS-SCRN-COUNTER = 0), set WS-NO-RECORDS-FOUND to TRUE.
11. If there are no more records to show, set CA-NEXT-PAGE-NOT-EXISTS to TRUE and update the error message accordingly.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 6
Here's the revised business rule incorporating all the details from the COBOL program:

```markdown
- ## Rule / Function / Method - Filter Records

- Detailed Description of the Rule:
  This rule applies the account ID and card number filters to determine if a record should be included in the display.

- What it proposes to do:
  Evaluate each record against the user-specified filters to decide whether to include it in the list of displayed credit cards.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Set WS-DONOT-EXCLUDE-THIS-RECORD to TRUE.
2. If FLG-ACCTFILTER-ISVALID is TRUE:
   a. If CARD-ACCT-ID equals CC-ACCT-ID, continue.
   b. Otherwise, set WS-EXCLUDE-THIS-RECORD to TRUE and exit the routine.
3. If FLG-CARDFILTER-ISVALID is TRUE:
   a. If CARD-NUM equals CC-CARD-NUM-N, continue.
   b. Otherwise, set WS-EXCLUDE-THIS-RECORD to TRUE and exit the routine.
4. If both filters pass or are not applied, the record will be included in the display.

## Additional Details:
- The account ID filter (CC-ACCT-ID) must be an 11-digit number if supplied.
- The card number filter (CC-CARD-NUM) must be a 16-digit number if supplied.
- If no filters are applied (both FLG-ACCTFILTER-ISVALID and FLG-CARDFILTER-ISVALID are FALSE), all records will be included.
- The routine is part of a larger process that reads records from a card file (CARDDAT) and displays them on a screen with a maximum of 7 lines.
- The filtering process is applied during both forward and backward reading of records.
- If no records match the filter criteria, an appropriate message will be displayed.
- The routine handles pagination, keeping track of the first and last card numbers displayed on each page.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 7
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Send Map

- Detailed Description of the Rule:
  This rule prepares and sends the screen map with the list of credit cards and other relevant information.

- What it proposes to do:
  Format the data for display, set up screen attributes, and send the formatted map to the user's terminal.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Perform Screen Initialization:
   a. Move low values to CCRDLIAO.
   b. Set up screen titles, transaction name, program name, current date, and time.
   c. Set the page number (WS-CA-SCREEN-NUM).
2. Perform Screen Array Initialization:
   a. For each row (1 to 7) in WS-SCREEN-ROWS:
      - If the row data is not low values:
        * Move WS-EDIT-SELECT to CRDSELnO.
        * Move WS-ROW-ACCTNO to ACCTNOnO.
        * Move WS-ROW-CARD-NUM to CRDNUMnO.
        * Move WS-ROW-CARD-STATUS to CRDSTSnO.
3. Setup Array Attributes:
   a. For each row (1 to 7):
      - Set appropriate display attributes (protected, unprotected, error highlighting) based on the row's status and user inputs.
      - If FLG-PROTECT-SELECT-ROWS-YES is set or the row data is low values, set the select field to protected.
      - If there's an error in the row's select field (WS-ROW-CRDSELECT-ERROR is '1'):
        * Set the select field color to red.
        * If the select field is empty, display an asterisk (*).
      - Otherwise, set the select field to unprotected.
4. Setup Screen Attributes:
   a. Initialize search criteria fields (ACCTSIDO, CARDSIDO) based on previous inputs or program state.
   b. Set cursor position and error highlighting for input fields if necessary.
   c. If FLG-ACCTFILTER-NOT-OK is set, highlight ACCTSID field in red and set cursor to it.
   d. If FLG-CARDFILTER-NOT-OK is set, highlight CARDSID field in red and set cursor to it.
   e. If no errors, position cursor at ACCTSID field.
5. Setup Message:
   a. Determine and set appropriate error or informational messages based on program state and user actions.
   b. Handle specific scenarios:
      - If on the first page and PF7 is pressed, display "NO PREVIOUS PAGES TO DISPLAY".
      - If on the last page and PF8 is pressed, display "NO MORE PAGES TO DISPLAY".
      - If no more records to show, display "NO MORE RECORDS TO SHOW".
      - If no records found for the search condition, display "NO RECORDS FOUND FOR THIS SEARCH CONDITION".
6. Send Screen:
   a. Execute CICS SEND MAP command with the following parameters:
      - Map name: 'CCRDLIA'
      - Mapset name: 'COCRDLI'
      - Data: CCRDLIAO
      - ERASE, CURSOR, and FREEKB options
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 8
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Process User Action

- Detailed Description of the Rule:
  This rule processes the user's action based on the function key pressed and the selections made on the screen.

- What it proposes to do:
  Determine the next action to take based on user input, such as navigating pages, viewing card details, or updating card information.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Evaluate the pressed function key (CCARD-AID):
   a. If PF3 (Exit):
      - Prepare to return to the main menu program (COMEN01C).
      - Set up the necessary fields in CARDDEMO-COMMAREA.
      - Execute CICS XCTL to transfer control to the menu program.
   b. If PF7 (Page Up) and not on the first page:
      - Perform 9100-READ-BACKWARDS to get the previous page of records.
      - Perform 1000-SEND-MAP to display the new page.
   c. If PF8 (Page Down) and more pages exist:
      - Perform 9000-READ-FORWARD to get the next page of records.
      - Perform 1000-SEND-MAP to display the new page.
   d. If Enter key and a row is selected for viewing (S):
      - Prepare to transfer control to the card detail view program (COCRDSLC).
      - Set up the necessary fields in CARDDEMO-COMMAREA, including the selected card's account ID and card number.
      - Execute CICS XCTL to transfer control to the card detail program.
   e. If Enter key and a row is selected for updating (U):
      - Prepare to transfer control to the card update program (COCRDUPC).
      - Set up the necessary fields in CARDDEMO-COMMAREA, including the selected card's account ID and card number.
      - Execute CICS XCTL to transfer control to the card update program.
   f. For any other case:
      - Perform 9000-READ-FORWARD to refresh the current page of records.
      - Perform 1000-SEND-MAP to redisplay the current page.
2. If any input errors occurred during processing:
   - Set up the appropriate error message.
   - Prepare to redisplay the current screen with error information.
3. Prepare the common area (CARDDEMO-COMMAREA and WS-THIS-PROGCOMMAREA) for the next interaction.
4. Execute CICS RETURN with TRANSID 'CCLI' to maintain program control for the next user action.
5. Additional details:
   - The program supports filtering by account ID and card number.
   - It allows selection of only one record at a time for viewing or updating.
   - The screen displays a maximum of 7 records at a time.
   - Error messages are displayed for invalid inputs, such as non-numeric account ID or card number.
   - The program handles various CICS responses, including NORMAL, DUPREC, and ENDFILE.
   - It uses STARTBR, READNEXT, READPREV, and ENDBR commands for navigating the card file.
   - The program maintains state information between interactions, including the current page number and the first/last card keys displayed.
   - It supports both admin users (who can see all cards) and regular users (who can only see cards associated with their account).
```

<!--rule-end-->