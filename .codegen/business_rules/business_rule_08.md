# COTRN01C.cbl: View a Transaction from TRANSACT file

## Overview
This CICS COBOL program, COTRN01C, is part of the CardDemo application. Its primary function is to view a transaction from the TRANSACT file. The program allows users to input a transaction ID and displays the corresponding transaction details on the screen.

The file plays a crucial role in the overall project by providing a user interface for viewing individual transaction records. It contributes to the system's functionality by allowing users to access and review specific transaction information, which is essential for monitoring and managing card transactions.

<!-- general-rule-start -->
## General Business Rules:

1. Transaction ID Validation:
   - The program validates that the transaction ID entered by the user is not empty.
   - If the transaction ID is empty, an error message is displayed.

2. Transaction Retrieval:
   - The program reads the TRANSACT file using the entered transaction ID.
   - If the transaction is found, its details are displayed on the screen.
   - If the transaction is not found, an error message is shown.

3. Screen Display:
   - The program displays a screen with fields for transaction details, including:
     - Transaction ID
     - Card Number
     - Transaction Type Code
     - Transaction Category Code
     - Transaction Source
     - Transaction Amount
     - Transaction Description
     - Original Transaction Timestamp
     - Processed Transaction Timestamp
     - Merchant ID
     - Merchant Name
     - Merchant City
     - Merchant ZIP

4. Navigation:
   - The program supports various function keys for navigation:
     - PF3: Return to the previous screen
     - PF4: Clear the current screen
     - PF5: Go to the transaction list screen (COTRN00C)

5. Error Handling:
   - The program displays appropriate error messages for invalid inputs or system errors.

6. Screen Header:
   - The program populates the screen header with current date, time, transaction ID, and program name.

7. Data Formatting:
   - The transaction amount is formatted for display purposes.

8. Program Flow:
   - The program follows a typical CICS transaction flow, including receiving user input, processing the request, and sending the response screen.

This program is essential for users who need to view detailed information about specific transactions, supporting functions such as transaction verification, dispute resolution, or general transaction monitoring within the CardDemo application.
<!-- general-rule-end -->
## Dependencies

This program relies on several dependencies, including copybooks for data structures, system libraries, and external files. These dependencies are crucial for the program's functionality and data handling. Understanding these dependencies is essential for any modernization efforts, as they define the data structures, screen layouts, and file interactions used by the program.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| COCOM01Y | Common communication area | Copybook | COPY COCOM01Y | Contains shared data structures; may need to be adapted or replaced in a modernized system |
| COTRN01 | Screen map for transaction view | Copybook | COPY COTRN01 | Defines the screen layout; may need to be replaced with a modern UI framework |
| COTTL01Y | Title information | Copybook | COPY COTTL01Y | Contains title information; may need to be centralized in a configuration file |
| CSDAT01Y | Date handling routines | Copybook | COPY CSDAT01Y | Date handling may need to be updated to use modern date libraries |
| CSMSG01Y | Message handling | Copybook | COPY CSMSG01Y | Message handling may need to be centralized or use a logging framework |
| CVTRA05Y | Transaction record structure | Copybook | COPY CVTRA05Y | Defines the transaction data structure; may need to be converted to a modern data format (e.g., JSON) |
| DFHAID | CICS AID constants | System Library | COPY DFHAID | CICS-specific; may need to be replaced with web-based input handling |
| DFHBMSCA | CICS BMS screen attributes | System Library | COPY DFHBMSCA | CICS-specific; may need to be replaced with web-based UI attributes |
| TRANSACT | Transaction file | External File | WS-TRANSACT-FILE | Data storage; may need to be migrated to a modern database system |
## Detailed Rules
## Data Structure

This section describes the key data structures used in the COTRN01C program, focusing on the transaction record and screen input/output fields. The data structure is crucial for understanding the information flow and storage in the application.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| TRAN-ID | Transaction ID | Alphanumeric | X(16) | Unique identifier for the transaction | Yes | None | Primary key for TRANSACT file | Yes | Input/Output |
| TRAN-CARD-NUM | Card Number | Alphanumeric | X(16) | Credit card number associated with the transaction | Yes | None | Masked for security | Yes | Output |
| TRAN-TYPE-CD | Transaction Type Code | Alphanumeric | X(02) | Code indicating the type of transaction | Yes | None | Refers to transaction type lookup | Yes | Output |
| TRAN-CAT-CD | Transaction Category Code | Numeric | 9(04) | Code indicating the category of the transaction | Yes | None | Refers to transaction category lookup | Yes | Output |
| TRAN-SOURCE | Transaction Source | Alphanumeric | X(10) | Source of the transaction | Yes | None | E.g., POS, ATM, ONLINE | Yes | Output |
| TRAN-AMT | Transaction Amount | Signed Numeric | S9(09)V99 | Amount of the transaction | Yes | None | Displayed with currency formatting | Yes | Output |
| TRAN-DESC | Transaction Description | Alphanumeric | X(100) | Detailed description of the transaction | No | Spaces | May contain merchant details | Yes | Output |
| TRAN-ORIG-TS | Original Transaction Timestamp | Alphanumeric | X(26) | Timestamp when the transaction was initiated | Yes | None | Format: YYYY-MM-DD-HH.MM.SS.MMMMMM | Yes | Output |
| TRAN-PROC-TS | Processed Transaction Timestamp | Alphanumeric | X(26) | Timestamp when the transaction was processed | Yes | None | Format: YYYY-MM-DD-HH.MM.SS.MMMMMM | Yes | Output |
| TRAN-MERCHANT-ID | Merchant ID | Numeric | 9(09) | Unique identifier for the merchant | Yes | None | Links to merchant database | Yes | Output |
| TRAN-MERCHANT-NAME | Merchant Name | Alphanumeric | X(50) | Name of the merchant | No | Spaces | Display name for the transaction | Yes | Output |
| TRAN-MERCHANT-CITY | Merchant City | Alphanumeric | X(50) | City where the merchant is located | No | Spaces | Additional transaction context | Yes | Output |
| TRAN-MERCHANT-ZIP | Merchant ZIP Code | Alphanumeric | X(10) | ZIP code of the merchant | No | Spaces | Used for geographical analysis | Yes | Output |
| TRNIDINI | Transaction ID Input | Alphanumeric | X(16) | User input field for transaction ID | Yes | Spaces | Used to search for a specific transaction | Yes | Input |
| WS-ERR-FLG | Error Flag | Alphanumeric | X(01) | Indicates if an error occurred during processing | No | 'N' | 'Y' for error, 'N' for no error | Yes | Internal |
| WS-MESSAGE | Message | Alphanumeric | X(80) | Holds error or information messages | No | Spaces | Displayed on the screen for user feedback | Yes | Output |



<!--rule-start-->
## Reviewed Rule 1
```markdown
- ## Rule / Function / Method - Name: PROCESS-ENTER-KEY

   - Detailed Description of the Rule:
     This function processes the user input when the Enter key is pressed on the transaction view screen. It validates the transaction ID input and retrieves the corresponding transaction details from the TRANSACT file.

   - What it proposes to do:
     Validate user input, retrieve transaction data, and display it on the screen.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Check if the transaction ID (TRNIDINI) is empty or contains only spaces:
      - If empty, set error flag (WS-ERR-FLG) to 'Y'
      - Set error message (WS-MESSAGE) to "Tran ID can NOT be empty..."
      - Set cursor position to TRNIDINI field
      - Call SEND-TRNVIEW-SCREEN to display the error
      - Exit the function

   2. If transaction ID is not empty:
      - Clear all output fields (TRNIDI, CARDNUMI, TTYPCDI, TCATCDI, TRNSRCI, TRNAMTI, TDESCI, TORIGDTI, TPROCDTI, MIDI, MNAMEI, MCITYI, MZIPI)
      - Move the input transaction ID (TRNIDINI) to TRAN-ID
      - Call READ-TRANSACT-FILE function

   3. If READ-TRANSACT-FILE is successful:
      - Format TRAN-AMT to WS-TRAN-AMT (for display purposes)
      - Move retrieved transaction data to corresponding screen fields:
        - TRAN-ID to TRNIDI
        - TRAN-CARD-NUM to CARDNUMI
        - TRAN-TYPE-CD to TTYPCDI
        - TRAN-CAT-CD to TCATCDI
        - TRAN-SOURCE to TRNSRCI
        - WS-TRAN-AMT to TRNAMTI
        - TRAN-DESC to TDESCI
        - TRAN-ORIG-TS to TORIGDTI
        - TRAN-PROC-TS to TPROCDTI
        - TRAN-MERCHANT-ID to MIDI
        - TRAN-MERCHANT-NAME to MNAMEI
        - TRAN-MERCHANT-CITY to MCITYI
        - TRAN-MERCHANT-ZIP to MZIPI
      - Call SEND-TRNVIEW-SCREEN to display the transaction details

   4. If READ-TRANSACT-FILE is unsuccessful:
      - If record not found:
        - Set error flag (WS-ERR-FLG) to 'Y'
        - Set error message (WS-MESSAGE) to "Transaction ID NOT found..."
        - Set cursor position to TRNIDINI field
        - Call SEND-TRNVIEW-SCREEN to display the error
      - For any other error:
        - Set error flag (WS-ERR-FLG) to 'Y'
        - Set error message (WS-MESSAGE) to "Unable to lookup Transaction..."
        - Set cursor position to TRNIDINI field
        - Call SEND-TRNVIEW-SCREEN to display the error

   5. The function uses a READ UPDATE operation when accessing the TRANSACT file, which could potentially lock the record for future updates.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
```markdown
- ## Rule / Function / Method - Name: READ-TRANSACT-FILE

   - Detailed Description of the Rule:
     This function reads a record from the TRANSACT file based on the provided transaction ID.

   - What it proposes to do:
     Retrieve transaction data from the TRANSACT file and handle potential errors.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Execute CICS READ command with the following parameters:
      - DATASET: WS-TRANSACT-FILE
      - INTO: TRAN-RECORD
      - LENGTH: LENGTH OF TRAN-RECORD
      - RIDFLD: TRAN-ID
      - KEYLENGTH: LENGTH OF TRAN-ID
      - UPDATE (allows for potential future updates)
      - RESP: WS-RESP-CD (to capture response code)
      - RESP2: WS-REAS-CD (to capture reason code)

   2. Evaluate the response code (WS-RESP-CD):
      - If DFHRESP(NORMAL):
        - Continue processing (record found and read successfully)
      - If DFHRESP(NOTFND):
        - Set error flag (WS-ERR-FLG) to 'Y'
        - Set error message (WS-MESSAGE) to "Transaction ID NOT found..."
        - Set cursor position to TRNIDINL field
        - Call SEND-TRNVIEW-SCREEN to display the error
      - For any other response:
        - Display RESP and REAS codes for debugging purposes
        - Set error flag (WS-ERR-FLG) to 'Y'
        - Set error message (WS-MESSAGE) to "Unable to lookup Transaction..."
        - Set cursor position to TRNIDINL field
        - Call SEND-TRNVIEW-SCREEN to display the error

   3. If no error occurred (WS-ERR-FLG is not 'Y'):
      - Move TRAN-AMT to WS-TRAN-AMT
      - Populate the following fields in COTRN1AI:
        - TRNIDI with TRAN-ID
        - CARDNUMI with TRAN-CARD-NUM
        - TTYPCDI with TRAN-TYPE-CD
        - TCATCDI with TRAN-CAT-CD
        - TRNSRCI with TRAN-SOURCE
        - TRNAMTI with WS-TRAN-AMT
        - TDESCI with TRAN-DESC
        - TORIGDTI with TRAN-ORIG-TS
        - TPROCDTI with TRAN-PROC-TS
        - MIDI with TRAN-MERCHANT-ID
        - MNAMEI with TRAN-MERCHANT-NAME
        - MCITYI with TRAN-MERCHANT-CITY
        - MZIPI with TRAN-MERCHANT-ZIP
      - Call SEND-TRNVIEW-SCREEN to display the populated data
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
```markdown
- ## Rule / Function / Method - Name: SEND-TRNVIEW-SCREEN

   - Detailed Description of the Rule:
     This function prepares and sends the transaction view screen to the user.

   - What it proposes to do:
     Populate the screen with header information and transaction details, then send it to the user's terminal.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Call POPULATE-HEADER-INFO to fill in the screen header with the following information:
      - Move CCDA-TITLE01 to TITLE01O of COTRN1AO
      - Move CCDA-TITLE02 to TITLE02O of COTRN1AO
      - Move WS-TRANID to TRNNAMEO of COTRN1AO
      - Move WS-PGMNAME to PGMNAMEO of COTRN1AO
      - Set current date (MM/DD/YY format) to CURDATEO of COTRN1AO
      - Set current time (HH:MM:SS format) to CURTIMEO of COTRN1AO

   2. Move WS-MESSAGE to ERRMSGO of COTRN1AO (to display any error messages)

   3. Execute CICS SEND command with the following parameters:
      - MAP: 'COTRN1A'
      - MAPSET: 'COTRN01'
      - FROM: COTRN1AO (the output area containing screen data)
      - ERASE (clear the screen before displaying new data)
      - CURSOR (position the cursor as specified)

   Note: This function is called after populating transaction details in COTRN1AO, which includes fields such as TRNIDI, CARDNUMI, TTYPCDI, TCATCDI, TRNSRCI, TRNAMTI, TDESCI, TORIGDTI, TPROCDTI, MIDI, MNAMEI, MCITYI, and MZIPI.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
- ## Rule / Function / Method - Name: POPULATE-HEADER-INFO

   - Detailed Description of the Rule:
     This function populates the header information for the transaction view screen.

   - What it proposes to do:
     Fill in the screen title, current date, time, transaction ID, and program name.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Get the current date and time:
      - Move FUNCTION CURRENT-DATE to WS-CURDATE-DATA

   2. Populate screen title:
      - Move CCDA-TITLE01 to TITLE01O of COTRN1AO
      - Move CCDA-TITLE02 to TITLE02O of COTRN1AO

   3. Populate transaction and program names:
      - Move WS-TRANID to TRNNAMEO of COTRN1AO
      - Move WS-PGMNAME to PGMNAMEO of COTRN1AO

   4. Format and populate current date:
      - Move WS-CURDATE-MONTH to WS-CURDATE-MM
      - Move WS-CURDATE-DAY to WS-CURDATE-DD
      - Move WS-CURDATE-YEAR(3:2) to WS-CURDATE-YY (last two digits of the year)
      - Move WS-CURDATE-MM-DD-YY to CURDATEO of COTRN1AO

   5. Format and populate current time:
      - Move WS-CURTIME-HOURS to WS-CURTIME-HH
      - Move WS-CURTIME-MINUTE to WS-CURTIME-MM
      - Move WS-CURTIME-SECOND to WS-CURTIME-SS
      - Move WS-CURTIME-HH-MM-SS to CURTIMEO of COTRN1AO

   6. Error handling:
      - If there are any errors during this process, they are not explicitly handled within this function. Error handling, if any, would be managed by the calling procedure.

   7. Output:
      - The function does not return any value. It directly updates the COTRN1AO structure with the populated header information.

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
```markdown
- ## Rule / Function / Method - Name: CLEAR-CURRENT-SCREEN

   - Detailed Description of the Rule:
     This function clears all fields on the current transaction view screen and redisplays it with empty fields.

   - What it proposes to do:
     Reset all input and output fields to their initial state and show a fresh screen to the user, maintaining the cursor position on the transaction ID input field.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Call INITIALIZE-ALL-FIELDS to reset all screen fields:
      - Set cursor position (-1) to the transaction ID input field (TRNIDINL)
      - Clear the following fields by setting them to spaces:
        - Transaction ID input (TRNIDINI)
        - Transaction ID display (TRNIDI)
        - Card Number (CARDNUMI)
        - Transaction Type Code (TTYPCDI)
        - Transaction Category Code (TCATCDI)
        - Transaction Source (TRNSRCI)
        - Transaction Amount (TRNAMTI)
        - Transaction Description (TDESCI)
        - Transaction Original Timestamp (TORIGDTI)
        - Transaction Processed Timestamp (TPROCDTI)
        - Merchant ID (MIDI)
        - Merchant Name (MNAMEI)
        - Merchant City (MCITYI)
        - Merchant ZIP (MZIPI)
      - Clear any error messages (WS-MESSAGE)

   2. Call SEND-TRNVIEW-SCREEN to display the cleared screen:
      - Populate header information (current date, time, transaction name, program name)
      - Send the map 'COTRN1A' from the mapset 'COTRN01' with the ERASE option
      - Position the cursor as specified in step 1
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 6
- ## Rule / Function / Method - Name: INITIALIZE-ALL-FIELDS

   - Detailed Description of the Rule:
     This function resets all fields on the transaction view screen to their initial values.

   - What it proposes to do:
     Clear all input and output fields, preparing the screen for new input.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Set cursor position:
      - Move -1 to TRNIDINL of COTRN1AI (position cursor on transaction ID input field)

   2. Clear all input and output fields:
      - Move SPACES to:
        - TRNIDINI of COTRN1AI (transaction ID input)
        - TRNIDI of COTRN1AI (transaction ID display)
        - CARDNUMI of COTRN1AI (card number)
        - TTYPCDI of COTRN1AI (transaction type code)
        - TCATCDI of COTRN1AI (transaction category code)
        - TRNSRCI of COTRN1AI (transaction source)
        - TRNAMTI of COTRN1AI (transaction amount)
        - TDESCI of COTRN1AI (transaction description)
        - TORIGDTI of COTRN1AI (original transaction date)
        - TPROCDTI of COTRN1AI (processed transaction date)
        - MIDI of COTRN1AI (merchant ID)
        - MNAMEI of COTRN1AI (merchant name)
        - MCITYI of COTRN1AI (merchant city)
        - MZIPI of COTRN1AI (merchant ZIP code)
        - WS-MESSAGE (clear any error or information messages)

   3. Note: This function does not reset any other variables in the WORKING-STORAGE SECTION or modify any other parts of the program state.

<!--rule-end-->