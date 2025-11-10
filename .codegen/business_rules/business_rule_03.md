# COCRDUPC.cbl: Credit Card Update Program

## Overview
This COBOL program, COCRDUPC, is part of a credit card management system. It handles the process of updating credit card details. The program allows users to view and modify information such as the card holder's name, card status, and expiration date for a specific credit card associated with an account.

<!-- general-rule-start -->
## General Business Rules:
1. The program allows users to search for a credit card using account ID and card number.
2. Once a card is found, the program displays its current details and allows modifications.
3. Editable fields include the card holder's name, card status (active/inactive), and expiration date (month and year).
4. The program performs various validations on the input data before allowing updates.
5. If changes are made, the program asks for confirmation before committing the updates to the database.
6. The program handles various scenarios such as record locking, concurrent modifications, and database errors.
7. It provides appropriate feedback messages to the user based on the operation's success or failure.
<!-- general-rule-end -->

Key functionalities include:
1. Searching for a credit card using account ID and card number
2. Displaying current card details
3. Allowing modifications to specific card fields
4. Validating user inputs
5. Confirming changes before updating the database
6. Handling various error scenarios and providing user feedback

The program interacts with a card file (CARDDAT) to read and update credit card records. It uses CICS commands for file operations and screen handling.

The main flow of the program involves:
1. Initializing the screen and handling user inputs
2. Validating search criteria and fetching card details
3. Displaying card information and allowing modifications
4. Validating changes and asking for confirmation
5. Updating the card record in the database
6. Providing appropriate feedback to the user

The program uses various status flags and condition names to manage the flow and track the state of the update process. It also includes error handling and abend routines to manage exceptional scenarios.
## Dependencies

This program relies on several external dependencies for its functionality. These include CICS commands for transaction handling and file operations, copybooks for shared data structures, and a card file for storing credit card information. Understanding these dependencies is crucial for any modernization efforts.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| CICS | Customer Information Control System, used for transaction processing and file operations | System | N/A | High - May need to be replaced or emulated in a modern environment |
| CARDDAT | Card data file used to store and retrieve credit card information | File | LIT-CARDFILENAME | High - Data storage mechanism may need to be updated |
| CVCRD01Y | Copybook containing common working storage variables | Copybook | COPY CVCRD01Y | Medium - May need to be refactored or replaced |
| COCOM01Y | Copybook containing application common area | Copybook | COPY COCOM01Y | Medium - May need to be refactored or replaced |
| COTTL01Y | Copybook for screen titles | Copybook | COPY COTTL01Y | Low - UI-related, may be replaced in web-based interface |
| COCRDUP | Copybook for credit card update screen layout | Copybook | COPY COCRDUP | Medium - Screen layout may need to be redesigned for web |
| CSDAT01Y | Copybook for current date | Copybook | COPY CSDAT01Y | Low - Date handling may be done differently in modern languages |
| CSMSG01Y | Copybook for common messages | Copybook | COPY CSMSG01Y | Low - Message handling may be centralized in modern systems |
| CSMSG02Y | Copybook for abend variables | Copybook | COPY CSMSG02Y | Medium - Error handling may be redesigned |
| CSUSR01Y | Copybook for signed-on user data | Copybook | COPY CSUSR01Y | Medium - User authentication may be handled differently |
| CVACT02Y | Copybook for card record layout | Copybook | COPY CVACT02Y | High - Core data structure, may need to be adapted |
| CVCUS01Y | Copybook for customer layout | Copybook | COPY CVCUS01Y | High - Core data structure, may need to be adapted |
| CSSTRPFY | Copybook for storing PFKey | Copybook | COPY 'CSSTRPFY' | Low - Function key handling may be replaced in web interface |
## Detailed Rules
## Data Structure

This section describes the key data structures used in the COCRDUPC program, focusing on the main fields relevant to credit card update operations.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| 1 | CC-ACCT-ID | Numeric | 11 | Account ID associated with the credit card | Yes | None | Used as part of the search key | High | Input/Output |
| 2 | CC-CARD-NUM | Numeric | 16 | Credit card number | Yes | None | Primary key for card lookup | High | Input/Output |
| 3 | CCUP-OLD-CRDNAME | Alphanumeric | 50 | Current cardholder name | Yes | None | Displayed for verification | Medium | Output |
| 4 | CCUP-NEW-CRDNAME | Alphanumeric | 50 | Updated cardholder name | Yes | None | User input for name change | Medium | Input |
| 5 | CCUP-OLD-CRDSTCD | Alphanumeric | 1 | Current card status | Yes | None | 'Y' for active, 'N' for inactive | High | Output |
| 6 | CCUP-NEW-CRDSTCD | Alphanumeric | 1 | Updated card status | Yes | None | User input for status change | High | Input |
| 7 | CCUP-OLD-EXPMON | Numeric | 2 | Current expiration month | Yes | None | Displayed for verification | Medium | Output |
| 8 | CCUP-NEW-EXPMON | Numeric | 2 | Updated expiration month | Yes | None | User input for expiry change | Medium | Input |
| 9 | CCUP-OLD-EXPYEAR | Numeric | 4 | Current expiration year | Yes | None | Displayed for verification | Medium | Output |
| 10 | CCUP-NEW-EXPYEAR | Numeric | 4 | Updated expiration year | Yes | None | User input for expiry change | Medium | Input |
| 11 | CARD-CVV-CD | Numeric | 3 | Card Verification Value | Yes | None | Not editable, for verification only | Low | Output |
| 12 | CARD-EXPIRAION-DATE | Alphanumeric | 10 | Formatted expiration date | Yes | None | Stored as 'YYYY-MM-DD' | Medium | Internal |
| 13 | CARD-ACTIVE-STATUS | Alphanumeric | 1 | Card active status in database | Yes | None | 'Y' for active, 'N' for inactive | High | Internal |
| 14 | WS-RESP-CD | Numeric | 9 | CICS response code | No | 0 | Used for error handling | Low | Internal |
| 15 | WS-REAS-CD | Numeric | 9 | CICS reason code | No | 0 | Used for error handling | Low | Internal |



<!--rule-start-->
## Reviewed Rule 1
Here's the revised business rule incorporating all the details from the COBOL program:

```markdown
- ## Rule / Function / Method - Search for Credit Card

- Detailed Description of the Rule:
  This rule allows the user to search for a specific credit card using the account ID and card number, and then update the card details.
- What it proposes to do:
  Retrieve the credit card details from the CARDDAT file based on the provided account ID and card number, and allow the user to update certain fields.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Receive input from the user:
   - Account ID (11-digit numeric)
   - Card Number (16-digit numeric)
2. Validate the input:
   - If Account ID is blank, all zeros, not numeric, or not 11 digits, set FLG-ACCTFILTER-NOT-OK
   - If Card Number is blank, all zeros, not numeric, or not 16 digits, set FLG-CARDFILTER-NOT-OK
3. If both inputs are valid:
   - Set WS-CARD-RID-CARDNUM to the input Card Number
   - Perform a CICS READ operation on the CARDDAT file using WS-CARD-RID-CARDNUM as the key
4. Handle the response from the CICS READ:
   - If RESP is DFHRESP(NORMAL), set FOUND-CARDS-FOR-ACCOUNT
   - If RESP is DFHRESP(NOTFND), set INPUT-ERROR, FLG-ACCTFILTER-NOT-OK, FLG-CARDFILTER-NOT-OK, and DID-NOT-FIND-ACCTCARD-COMBO
   - For any other RESP, set INPUT-ERROR, FLG-ACCTFILTER-NOT-OK, and populate WS-FILE-ERROR-MESSAGE
5. If the card is found, populate the CCUP-OLD-DETAILS with the retrieved data:
   - CCUP-OLD-ACCTID (Account ID)
   - CCUP-OLD-CARDID (Card Number)
   - CCUP-OLD-CVV-CD (CVV Code)
   - CCUP-OLD-CRDNAME (Card Name, converted to uppercase)
   - CCUP-OLD-EXPYEAR (Expiration Year)
   - CCUP-OLD-EXPMON (Expiration Month)
   - CCUP-OLD-EXPDAY (Expiration Day)
   - CCUP-OLD-CRDSTCD (Card Status)
6. Display the card details to the user and allow updates to the following fields:
   - Card Name (50 characters, alphabets and spaces only)
   - Card Status (1 character, 'Y' or 'N' only)
   - Expiration Month (2 digits, 01-12 only)
   - Expiration Year (4 digits, 1950-2099 only)
7. Validate the user's input:
   - If Card Name is blank or contains non-alphabetic characters (excluding spaces), set FLG-CARDNAME-NOT-OK
   - If Card Status is blank or not 'Y' or 'N', set FLG-CARDSTATUS-NOT-OK
   - If Expiration Month is blank or not between 01 and 12, set FLG-CARDEXPMON-NOT-OK
   - If Expiration Year is blank or not between 1950 and 2099, set FLG-CARDEXPYEAR-NOT-OK
8. If all inputs are valid:
   - Set CCUP-CHANGES-OK-NOT-CONFIRMED
   - Prompt user for confirmation to save changes
9. If user confirms (PF5 key):
   - Perform a CICS READ UPDATE on the CARDDAT file
   - If unable to lock the record, set COULD-NOT-LOCK-FOR-UPDATE and exit
   - Check if the record was changed by someone else by comparing with CCUP-OLD-DETAILS
   - If changed, set DATA-WAS-CHANGED-BEFORE-UPDATE and exit
   - If not changed, update the CARD-UPDATE-RECORD with the new details
   - Perform a CICS REWRITE operation on the CARDDAT file
   - If successful, set CCUP-CHANGES-OKAYED-AND-DONE
   - If unsuccessful, set LOCKED-BUT-UPDATE-FAILED
10. Display appropriate success or error messages to the user
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
- ## Rule / Function / Method - Display Card Details

- Detailed Description of the Rule:
  This rule displays the current credit card details on the screen for the user to view and potentially modify.
- What it proposes to do:
  Present the retrieved credit card information in an editable format on the screen.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Set CCUP-SHOW-DETAILS to TRUE
2. Populate the screen fields with data from CCUP-OLD-DETAILS:
   - Set ACCTSIDO of CCRDUPAO to CCUP-OLD-ACCTID
   - Set CARDSIDO of CCRDUPAO to CCUP-OLD-CARDID
   - Set CRDNAMEO of CCRDUPAO to CCUP-OLD-CRDNAME
   - Set CRDSTCDO of CCRDUPAO to CCUP-OLD-CRDSTCD
   - Set EXPDAYO of CCRDUPAO to CCUP-OLD-EXPDAY
   - Set EXPMONO of CCRDUPAO to CCUP-OLD-EXPMON
   - Set EXPYEARO of CCRDUPAO to CCUP-OLD-EXPYEAR
3. Set up screen attributes:
   - Set ACCTSIDA and CARDSIDA of CCRDUPAI to DFHBMPRF (protected field)
   - Set CRDNAMEA, CRDSTCDA, EXPMONA, and EXPYEARA of CCRDUPAI to DFHBMFSE (unprotected field)
4. Position the cursor on the CRDNAMEL field
5. Set FOUND-CARDS-FOR-ACCOUNT in WS-INFO-MSG
6. Perform the CICS SEND MAP operation to display the screen
<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - Validate User Input

- Detailed Description of the Rule:
  This rule validates the user input for card details to ensure data integrity.
- What it proposes to do:
  Check if the entered data meets the required format and business rules.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Validate Card Name (CCUP-NEW-CRDNAME):
   - If blank, set FLG-CARDNAME-BLANK and WS-PROMPT-FOR-NAME
   - Check if it contains only alphabets and spaces:
     - Convert all characters to spaces using INSPECT with CONVERTING
     - If the resulting string is not all spaces, set FLG-CARDNAME-NOT-OK and WS-NAME-MUST-BE-ALPHA
   - If valid, set FLG-CARDNAME-ISVALID

2. Validate Card Status (CCUP-NEW-CRDSTCD):
   - If blank, set FLG-CARDSTATUS-BLANK and CARD-STATUS-MUST-BE-YES-NO
   - Check if it's 'Y' or 'N':
     - Move CCUP-NEW-CRDSTCD to FLG-YES-NO-CHECK
     - If FLG-YES-NO-VALID is not set, set FLG-CARDSTATUS-NOT-OK and CARD-STATUS-MUST-BE-YES-NO
   - If valid, set FLG-CARDSTATUS-ISVALID

3. Validate Expiry Month (CCUP-NEW-EXPMON):
   - If blank, set FLG-CARDEXPMON-BLANK and CARD-EXPIRY-MONTH-NOT-VALID
   - Check if it's a number between 1 and 12:
     - Move CCUP-NEW-EXPMON to CARD-MONTH-CHECK
     - If VALID-MONTH (values 1 through 12) is not set, set FLG-CARDEXPMON-NOT-OK and CARD-EXPIRY-MONTH-NOT-VALID
   - If valid, set FLG-CARDEXPMON-ISVALID

4. Validate Expiry Year (CCUP-NEW-EXPYEAR):
   - If blank, set FLG-CARDEXPYEAR-BLANK and CARD-EXPIRY-YEAR-NOT-VALID
   - Check if it's a number between 1950 and 2099:
     - Move CCUP-NEW-EXPYEAR to CARD-YEAR-CHECK
     - If VALID-YEAR (values 1950 through 2099) is not set, set FLG-CARDEXPYEAR-NOT-OK and CARD-EXPIRY-YEAR-NOT-VALID
   - If valid, set FLG-CARDEXPYEAR-ISVALID

5. If any validation fails:
   - Set INPUT-ERROR
   - Set CCUP-CHANGES-NOT-OK

6. If all validations pass:
   - Set CCUP-CHANGES-OK-NOT-CONFIRMED

7. If no changes are detected (FUNCTION UPPER-CASE(CCUP-NEW-CARDDATA) EQUAL FUNCTION UPPER-CASE(CCUP-OLD-CARDDATA)):
   - Set NO-CHANGES-DETECTED
   - Set FLG-CARDNAME-ISVALID, FLG-CARDSTATUS-ISVALID, FLG-CARDEXPMON-ISVALID, and FLG-CARDEXPYEAR-ISVALID

8. If changes are confirmed (CCARD-AID-PFK05 and CCUP-CHANGES-OK-NOT-CONFIRMED):
   - Perform write processing:
     - Read and lock the card record
     - Check if the record was changed by someone else
     - If changed, set DATA-WAS-CHANGED-BEFORE-UPDATE and exit
     - If not changed, update the record with new values
     - If update fails, set LOCKED-BUT-UPDATE-FAILED
     - If update succeeds, set CCUP-CHANGES-OKAYED-AND-DONE

9. Handle various scenarios:
   - If CCUP-CHANGES-OKAYED-AND-DONE, reset search keys and ask for fresh search criteria
   - If CCUP-CHANGES-FAILED, reset search keys and ask for fresh search criteria
   - If CCARD-AID-PFK12, reset to show details again

10. Set appropriate error messages and screen attributes based on validation results and user actions
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - Update Card Details

- Detailed Description of the Rule:
  This rule updates the credit card details in the CARDDAT file after user confirmation.
- What it proposes to do:
  Write the modified card information back to the database.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Perform a CICS READ UPDATE operation on the CARDDAT file using WS-CARD-RID-CARDNUM (16-digit card number) as the key
2. If the READ UPDATE is successful (RESP is DFHRESP(NORMAL)), continue; otherwise, set COULD-NOT-LOCK-FOR-UPDATE and exit

3. Check if the record was changed by another user:
   - Compare each field in the read record with the corresponding CCUP-OLD-* fields:
     - CARD-CVV-CD with CCUP-OLD-CVV-CD
     - CARD-EMBOSSED-NAME with CCUP-OLD-CRDNAME (after converting CARD-EMBOSSED-NAME to uppercase)
     - CARD-EXPIRAION-DATE(1:4) with CCUP-OLD-EXPYEAR
     - CARD-EXPIRAION-DATE(6:2) with CCUP-OLD-EXPMON
     - CARD-EXPIRAION-DATE(9:2) with CCUP-OLD-EXPDAY
     - CARD-ACTIVE-STATUS with CCUP-OLD-CRDSTCD
   - If any field differs, set DATA-WAS-CHANGED-BEFORE-UPDATE, update CCUP-OLD-* fields with new values, and exit

4. Prepare the update record (CARD-UPDATE-RECORD):
   - Set CARD-UPDATE-NUM to CCUP-NEW-CARDID
   - Set CARD-UPDATE-ACCT-ID to CC-ACCT-ID-N
   - Set CARD-UPDATE-CVV-CD to CCUP-NEW-CVV-CD (converted to numeric)
   - Set CARD-UPDATE-EMBOSSED-NAME to CCUP-NEW-CRDNAME
   - Set CARD-UPDATE-EXPIRAION-DATE to concatenated CCUP-NEW-EXPYEAR, CCUP-NEW-EXPMON, and CCUP-NEW-EXPDAY with hyphens
   - Set CARD-UPDATE-ACTIVE-STATUS to CCUP-NEW-CRDSTCD

5. Perform a CICS REWRITE operation on the CARDDAT file with the CARD-UPDATE-RECORD

6. Check the RESP of the REWRITE operation:
   - If RESP is DFHRESP(NORMAL), the update was successful
   - Otherwise, set LOCKED-BUT-UPDATE-FAILED

7. If the update was successful, set CCUP-CHANGES-OKAYED-AND-DONE; otherwise, set appropriate error flags

8. Additional details:
   - The program converts all card names to uppercase before comparison and storage
   - The program does not allow changes to the expiry day (CCUP-NEW-EXPDAY is not used, CCUP-OLD-EXPDAY is used instead)
   - The program performs various input validations before attempting the update, including:
     - Card name must contain only alphabets and spaces
     - Card status must be 'Y' or 'N'
     - Expiry month must be between 1 and 12
     - Expiry year must be between 1950 and 2099
   - If any validation fails, the update process is not initiated
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - Handle User Navigation

- Detailed Description of the Rule:
  This rule manages the user's navigation through the program, including exiting, canceling operations, and processing credit card updates.
- What it proposes to do:
  Control the flow of the program based on user actions, function key presses, and data input validation.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Check the AID (Attention Identifier) after each user interaction:
   - If CCARD-AID-PFK03 (usually F3) is pressed:
     - Set CCARD-AID-PFK03 to TRUE
     - Determine the program to return to (CDEMO-TO-PROGRAM):
       - If CDEMO-FROM-PROGRAM is blank, set to LIT-MENUPGM ('COMEN01C')
       - Otherwise, set to CDEMO-FROM-PROGRAM
     - Set CDEMO-FROM-TRANID to LIT-THISTRANID ('CCUP')
     - Set CDEMO-FROM-PROGRAM to LIT-THISPGM ('COCRDUPC')
     - If returning to the card list (CDEMO-LAST-MAPSET is LIT-CCLISTMAPSET 'COCRDLI'), reset CDEMO-ACCT-ID and CDEMO-CARD-NUM to zeros
     - Perform a CICS SYNCPOINT
     - Perform a CICS XCTL to CDEMO-TO-PROGRAM with CARDDEMO-COMMAREA

2. If CCARD-AID-PFK12 (usually F12) is pressed and not CCUP-DETAILS-NOT-FETCHED:
   - Set CDEMO-PGM-REENTER to TRUE
   - Set INPUT-OK to TRUE
   - Set FLG-ACCTFILTER-ISVALID and FLG-CARDFILTER-ISVALID to TRUE
   - Move CDEMO-ACCT-ID to CC-ACCT-ID-N
   - Move CDEMO-CARD-NUM to CC-CARD-NUM-N
   - Perform 9000-READ-DATA to fetch the card details again
   - Set CCUP-SHOW-DETAILS to TRUE

3. If CCARD-AID-PFK05 (usually F5) is pressed and CCUP-CHANGES-OK-NOT-CONFIRMED:
   - Perform the update process (9200-WRITE-PROCESSING)
   - Based on the result, set appropriate flags:
     - CCUP-CHANGES-OKAYED-AND-DONE
     - CCUP-CHANGES-OKAYED-LOCK-ERROR
     - CCUP-CHANGES-OKAYED-BUT-FAILED
     - If DATA-WAS-CHANGED-BEFORE-UPDATE, set CCUP-SHOW-DETAILS to TRUE

4. For any other AID, treat it as CCARD-AID-ENTER and process accordingly

5. After processing, perform 3000-SEND-MAP to display the appropriate screen based on the current state

6. Input Validation:
   - Account ID: Must be a non-zero 11-digit number
   - Card Number: Must be a 16-digit number
   - Card Name: Must contain only alphabets and spaces
   - Card Status: Must be 'Y' or 'N'
   - Expiry Month: Must be between 1 and 12
   - Expiry Year: Must be between 1950 and 2099

7. Error Handling:
   - Display appropriate error messages for invalid inputs
   - Handle database read/write errors and display relevant messages

8. Screen Display:
   - Show/hide fields based on the current state of the transaction
   - Highlight error fields in red
   - Display informational messages based on the current state and user actions

9. Data Processing:
   - Convert card name to uppercase before storing
   - Format expiration date as 'YYYY-MM-DD' for storage
   - Check for data changes before updating the record

10. Navigation:
    - Allow returning to the main menu or previous screen based on the program flow
    - Reset search criteria when exiting to the card list screen

11. Perform a final CICS RETURN with the updated COMMAREA containing all transaction data
```

<!--rule-end-->