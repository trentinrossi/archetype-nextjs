# COTRN02C.CBL: Add a New Transaction to TRANSACT File

## Overview
This CICS COBOL program, COTRN02C, is part of the CardDemo application. Its primary function is to add a new transaction to the TRANSACT file. The program handles user input, validates the data, and writes the new transaction record to the file.

## General Business Rules:
<!-- general-rule-start -->
1. The program allows users to add new transactions to the TRANSACT file.
2. It validates input fields for completeness and correctness.
3. The program can retrieve account information using either an Account ID or a Card Number.
4. It performs date validation for transaction dates.
5. The program generates a unique Transaction ID for each new transaction.
6. It allows users to copy data from the last transaction for convenience.
7. The program updates the TRANSACT file with the new transaction record.
8. It provides feedback to the user about the success or failure of the transaction addition.
<!-- general-rule-end -->

Key functionalities include:

1. Input Validation:
   - Checks for empty fields
   - Validates numeric fields
   - Ensures correct date formats (YYYY-MM-DD)
   - Verifies amount format (-99999999.99)

2. Account/Card Lookup:
   - Reads CXACAIX file to get card number from account ID
   - Reads CCXREF file to get account ID from card number

3. Transaction ID Generation:
   - Reads the last transaction record
   - Increments the transaction ID for the new record

4. File Operations:
   - Writes the new transaction record to the TRANSACT file

5. User Interface:
   - Displays error messages for invalid inputs
   - Shows success message with the new transaction ID
   - Allows clearing of the current screen
   - Enables copying of last transaction data

6. Navigation:
   - Handles various function keys (PF keys) for different actions
   - Allows returning to the previous screen

The program interacts with multiple files:
- TRANSACT: Main transaction file
- ACCTDAT: Account data file
- CCXREF: Card cross-reference file
- CXACAIX: Account-Card cross-reference file

It uses several copybooks for data structures and common functionalities, ensuring consistency across the application.
## Dependencies

This program relies on several external dependencies, including CICS commands, copybooks, and file systems. These dependencies are crucial for the program's functionality and will need to be considered in any modernization efforts.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| CICS | Customer Information Control System | Runtime Environment | N/A | High - Core transaction processing system |
| COCOM01Y | Common area copybook | Copybook | COCOM01Y | Medium - Contains shared data structures |
| COTRN02 | Screen map for transaction addition | BMS Map | COTRN02 | High - User interface definition |
| COTTL01Y | Title information copybook | Copybook | COTTL01Y | Low - Contains static title information |
| CSDAT01Y | Date-related copybook | Copybook | CSDAT01Y | Medium - Date handling routines |
| CSMSG01Y | Message-related copybook | Copybook | CSMSG01Y | Medium - Message handling |
| CVTRA05Y | Transaction record structure | Copybook | CVTRA05Y | High - Core data structure for transactions |
| CVACT01Y | Account record structure | Copybook | CVACT01Y | High - Core data structure for accounts |
| CVACT03Y | Card cross-reference structure | Copybook | CVACT03Y | High - Links cards to accounts |
| DFHAID | CICS AID constants | Copybook | DFHAID | Medium - Defines function key constants |
| DFHBMSCA | CICS BMS attributes | Copybook | DFHBMSCA | Medium - Defines screen attributes |
| TRANSACT | Transaction file | VSAM File | WS-TRANSACT-FILE | High - Main data store for transactions |
| ACCTDAT | Account data file | VSAM File | WS-ACCTDAT-FILE | High - Stores account information |
| CCXREF | Card cross-reference file | VSAM File | WS-CCXREF-FILE | High - Links cards to accounts |
| CXACAIX | Account-Card cross-reference file | VSAM File | WS-CXACAIX-FILE | High - Alternative index for account-card lookup |
| CSUTLDTC | Date conversion utility | External Program | CSUTLDTC | Medium - Used for date validation |

These dependencies play crucial roles in the program's operation:

1. CICS provides the runtime environment and essential commands for transaction processing and file operations.
2. Copybooks define data structures and common routines used across the application.
3. BMS maps define the user interface for data entry and display.
4. VSAM files store and retrieve transaction, account, and card data.
5. The external date conversion utility (CSUTLDTC) is used for date validation.

In a modernization effort, these dependencies would need to be carefully considered. The VSAM files might be replaced with a relational or NoSQL database, CICS commands could be substituted with a modern application server, and the BMS maps might be replaced with a web-based user interface. The business logic contained in the copybooks and the main program would likely need to be refactored into a more modular, object-oriented structure.
## Detailed Rules
## Data Structure

This section describes the key data structures used in the COTRN02C program, focusing on the main transaction record and related fields.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| TRAN-ID | Transaction ID | Numeric | 16 | Unique identifier for the transaction | Yes | None | Generated automatically | High | Key |
| TRAN-TYPE-CD | Transaction Type Code | Alphanumeric | 2 | Code indicating the type of transaction | Yes | None | | High | Categorical |
| TRAN-CAT-CD | Transaction Category Code | Numeric | 4 | Code for transaction category | Yes | None | | High | Categorical |
| TRAN-SOURCE | Transaction Source | Alphanumeric | 10 | Source of the transaction | Yes | None | | Medium | Descriptive |
| TRAN-DESC | Transaction Description | Alphanumeric | 100 | Detailed description of the transaction | Yes | None | | High | Descriptive |
| TRAN-AMT | Transaction Amount | Numeric | 11 (9.2) | Monetary value of the transaction | Yes | None | Signed field | High | Numeric |
| TRAN-MERCHANT-ID | Merchant ID | Numeric | 9 | Unique identifier for the merchant | Yes | None | | High | Key |
| TRAN-MERCHANT-NAME | Merchant Name | Alphanumeric | 50 | Name of the merchant | Yes | None | | Medium | Descriptive |
| TRAN-MERCHANT-CITY | Merchant City | Alphanumeric | 50 | City of the merchant | Yes | None | | Medium | Descriptive |
| TRAN-MERCHANT-ZIP | Merchant ZIP | Alphanumeric | 10 | ZIP code of the merchant | Yes | None | | Medium | Descriptive |
| TRAN-CARD-NUM | Card Number | Alphanumeric | 16 | Credit card number associated with the transaction | Yes | None | | High | Key |
| TRAN-ORIG-TS | Original Timestamp | Alphanumeric | 26 | Timestamp of when the transaction originated | Yes | None | Format: YYYY-MM-DD | High | Temporal |
| TRAN-PROC-TS | Process Timestamp | Alphanumeric | 26 | Timestamp of when the transaction was processed | Yes | None | Format: YYYY-MM-DD | High | Temporal |
| XREF-ACCT-ID | Account ID | Numeric | 11 | Unique identifier for the account | Yes | None | Used in cross-reference lookups | High | Key |
| XREF-CARD-NUM | Card Number | Alphanumeric | 16 | Credit card number | Yes | None | Used in cross-reference lookups | High | Key |
| WS-TRAN-AMT-N | Transaction Amount (Numeric) | Numeric | 11 (9.2) | Internal numeric representation of transaction amount | No | 0 | Used for calculations | Medium | Numeric |
| WS-TRAN-AMT-E | Transaction Amount (Edited) | Alphanumeric | 12 | Edited representation of transaction amount | No | Zeros | Format: +99999999.99 | Medium | Numeric |
| WS-DATE-FORMAT | Date Format | Alphanumeric | 10 | Format string for date validation | No | 'YYYY-MM-DD' | Used in date validation | Low | Constant |
| WS-ERR-FLG | Error Flag | Alphanumeric | 1 | Indicates presence of errors | No | 'N' | 'Y' for error, 'N' for no error | Medium | Control |
| WS-MESSAGE | Message | Alphanumeric | 80 | Holds error or success messages | No | Spaces | | Medium | Informational |

This data structure represents the core elements of a transaction in the system. It includes fields for transaction identification, categorization, financial details, merchant information, and temporal data. The structure also includes cross-reference fields for account and card lookups, as well as working storage fields used in processing and error handling. All these fields are highly relevant for modernization efforts, as they represent the essential data model for transactions in the system.



<!--rule-start-->
## Reviewed Rule 1
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - Validate Input Key Fields:
   - Detailed Description of the Rule: This function validates the input key fields (Account ID or Card Number) to ensure that at least one of them is provided and is valid.
   - What it proposes to do: Ensure that the user has entered either a valid Account ID or a valid Card Number to proceed with the transaction addition.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Check if Account ID (ACTIDINI) is not spaces and not low-values:
      - If Account ID is not numeric:
        - Set error flag to 'Y'
        - Set error message to 'Account ID must be Numeric...'
        - Set cursor position to ACTIDINL
        - Perform SEND-TRNADD-SCREEN
      - Convert Account ID to numeric value using FUNCTION NUMVAL
      - Move the numeric value to WS-ACCT-ID-N and ACTIDINI
      - Move WS-ACCT-ID-N to XREF-ACCT-ID
      - Perform READ-CXACAIX-FILE to get the corresponding card number
        - If READ-CXACAIX-FILE returns NOTFND:
          - Set error flag to 'Y'
          - Set error message to 'Account ID NOT found...'
          - Set cursor position to ACTIDINL
          - Perform SEND-TRNADD-SCREEN
        - If READ-CXACAIX-FILE returns any other error:
          - Set error flag to 'Y'
          - Set error message to 'Unable to lookup Acct in XREF AIX file...'
          - Set cursor position to ACTIDINL
          - Perform SEND-TRNADD-SCREEN
      - Move the retrieved card number to CARDNINI
   2. If Account ID is not provided, check if Card Number (CARDNINI) is not spaces and not low-values:
      - If Card Number is not numeric:
        - Set error flag to 'Y'
        - Set error message to 'Card Number must be Numeric...'
        - Set cursor position to CARDNINL
        - Perform SEND-TRNADD-SCREEN
      - Convert Card Number to numeric value using FUNCTION NUMVAL
      - Move the numeric value to WS-CARD-NUM-N and CARDNINI
      - Move WS-CARD-NUM-N to XREF-CARD-NUM
      - Perform READ-CCXREF-FILE to get the corresponding account ID
        - If READ-CCXREF-FILE returns NOTFND:
          - Set error flag to 'Y'
          - Set error message to 'Card Number NOT found...'
          - Set cursor position to CARDNINL
          - Perform SEND-TRNADD-SCREEN
        - If READ-CCXREF-FILE returns any other error:
          - Set error flag to 'Y'
          - Set error message to 'Unable to lookup Card # in XREF file...'
          - Set cursor position to CARDNINL
          - Perform SEND-TRNADD-SCREEN
      - Move the retrieved account ID to ACTIDINI
   3. If neither Account ID nor Card Number is provided:
      - Set error flag to 'Y'
      - Set error message to 'Account or Card Number must be entered...'
      - Set cursor position to ACTIDINL
      - Perform SEND-TRNADD-SCREEN
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Validate Input Data Fields:
   - Detailed Description of the Rule: This function validates all input data fields for the new transaction to ensure they are complete and in the correct format.
   - What it proposes to do: Ensure that all required fields are filled and that the data is in the correct format before adding the transaction.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Check if each of the following fields is not spaces or low-values:
      - TTYPCDI (Transaction Type Code)
      - TCATCDI (Transaction Category Code)
      - TRNSRCI (Transaction Source)
      - TDESCI (Transaction Description)
      - TRNAMTI (Transaction Amount)
      - TORIGDTI (Transaction Original Date)
      - TPROCDTI (Transaction Process Date)
      - MIDI (Merchant ID)
      - MNAMEI (Merchant Name)
      - MCITYI (Merchant City)
      - MZIPI (Merchant ZIP)
   2. For each empty field:
      - Set error flag (WS-ERR-FLG) to 'Y'
      - Set appropriate error message in WS-MESSAGE
      - Set cursor position to the corresponding field
      - Perform SEND-TRNADD-SCREEN
   3. Validate numeric fields:
      - Check if TTYPCDI and TCATCDI are numeric
      - If not, set error flag, message, and cursor position
   4. Validate Transaction Amount (TRNAMTI):
      - Check if it matches the format -99999999.99 or +99999999.99
      - First character must be '-' or '+'
      - Characters 2-9 must be numeric
      - Character 10 must be '.'
      - Characters 11-12 must be numeric
      - If format is incorrect, set error flag, message, and cursor position
   5. Validate Original Date (TORIGDTI) and Process Date (TPROCDTI):
      - Check if they match the format YYYY-MM-DD
      - Characters 1-4 must be numeric (year)
      - Character 5 must be '-'
      - Characters 6-7 must be numeric (month)
      - Character 8 must be '-'
      - Characters 9-10 must be numeric (day)
      - If format is incorrect, set error flag, message, and cursor position
   6. Convert Transaction Amount to numeric:
      - Use FUNCTION NUMVAL-C to convert TRNAMTI to numeric
      - Move the result to WS-TRAN-AMT-N and WS-TRAN-AMT-E
      - Move WS-TRAN-AMT-E back to TRNAMTI
   7. Validate dates using CSUTLDTC utility:
      - For both TORIGDTI and TPROCDTI:
        - Move date to CSUTLDTC-DATE
        - Move 'YYYY-MM-DD' to CSUTLDTC-DATE-FORMAT
        - Call 'CSUTLDTC' with CSUTLDTC-DATE, CSUTLDTC-DATE-FORMAT, and CSUTLDTC-RESULT
        - If CSUTLDTC-RESULT-SEV-CD is not '0000' and CSUTLDTC-RESULT-MSG-NUM is not '2513':
          - Set error flag, appropriate error message, and cursor position
   8. Validate Merchant ID (MIDI):
      - Check if it's numeric
      - If not, set error flag, message, and cursor position
   9. If any error is found (WS-ERR-FLG is 'Y'):
      - Clear all input fields (set to SPACES)
   10. If no errors are found, proceed with adding the transaction
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - Add Transaction:
   - Detailed Description of the Rule: This function adds a new transaction to the TRANSACT file after all validations are successful.
   - What it proposes to do: Create a new transaction record and write it to the TRANSACT file.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Validate input key fields:
      - Either Account ID or Card Number must be entered
      - If Account ID is entered:
        - Verify it's numeric
        - Read CXACAIX file to get corresponding Card Number
      - If Card Number is entered:
        - Verify it's numeric
        - Read CCXREF file to get corresponding Account ID
   2. Validate input data fields:
      - Ensure all fields are not empty: Type CD, Category CD, Source, Description, Amount, Orig Date, Proc Date, Merchant ID, Merchant Name, Merchant City, Merchant Zip
      - Verify Type CD and Category CD are numeric
      - Verify Amount format: (+/-)99999999.99
      - Verify Orig Date and Proc Date format: YYYY-MM-DD
      - Verify Merchant ID is numeric
      - Convert Amount to internal numeric format
   3. Generate new Transaction ID:
      - Move HIGH-VALUES to TRAN-ID
      - Perform STARTBR-TRANSACT-FILE
      - Perform READPREV-TRANSACT-FILE to get the last transaction
      - Perform ENDBR-TRANSACT-FILE
      - Move TRAN-ID to WS-TRAN-ID-N
      - Add 1 to WS-TRAN-ID-N to generate the new Transaction ID
   4. Initialize TRAN-RECORD
   5. Populate TRAN-RECORD fields:
      - Move WS-TRAN-ID-N to TRAN-ID
      - Move TTYPCDI to TRAN-TYPE-CD
      - Move TCATCDI to TRAN-CAT-CD
      - Move TRNSRCI to TRAN-SOURCE
      - Move TDESCI to TRAN-DESC
      - Convert TRNAMTI to numeric using FUNCTION NUMVAL-C and move to TRAN-AMT
      - Move CARDNINI to TRAN-CARD-NUM
      - Move MIDI to TRAN-MERCHANT-ID
      - Move MNAMEI to TRAN-MERCHANT-NAME
      - Move MCITYI to TRAN-MERCHANT-CITY
      - Move MZIPI to TRAN-MERCHANT-ZIP
      - Move TORIGDTI to TRAN-ORIG-TS
      - Move TPROCDTI to TRAN-PROC-TS
   6. Perform WRITE-TRANSACT-FILE to add the new record
   7. Handle the result of WRITE-TRANSACT-FILE:
      - If successful (RESP = DFHRESP(NORMAL)):
        - Perform INITIALIZE-ALL-FIELDS
        - Set success message with new Transaction ID
        - Perform SEND-TRNADD-SCREEN
      - If duplicate key (RESP = DFHRESP(DUPKEY) or DFHRESP(DUPREC)):
        - Set error flag, message "Tran ID already exist...", and cursor position
        - Perform SEND-TRNADD-SCREEN
      - For other responses:
        - Set error flag, message "Unable to Add Transaction...", and cursor position
        - Perform SEND-TRNADD-SCREEN
   8. Additional features:
      - PF3: Return to previous screen
      - PF4: Clear current screen
      - PF5: Copy last transaction data
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
- ## Rule / Function / Method - Copy Last Transaction Data:
   - Detailed Description of the Rule: This function copies data from the last transaction to the input fields, allowing the user to quickly enter similar transactions.
   - What it proposes to do: Populate the input fields with data from the most recent transaction for the given account or card.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Perform VALIDATE-INPUT-KEY-FIELDS to ensure a valid account or card is selected
   2. Move HIGH-VALUES to TRAN-ID
   3. Perform STARTBR-TRANSACT-FILE
   4. Perform READPREV-TRANSACT-FILE to get the last transaction
   5. Perform ENDBR-TRANSACT-FILE
   6. If no error flag is set:
      - Move TRAN-AMT to WS-TRAN-AMT-E
      - Copy data from TRAN-RECORD to input fields:
        - Move TRAN-TYPE-CD to TTYPCDI
        - Move TRAN-CAT-CD to TCATCDI
        - Move TRAN-SOURCE to TRNSRCI
        - Move WS-TRAN-AMT-E to TRNAMTI
        - Move TRAN-DESC to TDESCI
        - Move TRAN-ORIG-TS to TORIGDTI
        - Move TRAN-PROC-TS to TPROCDTI
        - Move TRAN-MERCHANT-ID to MIDI
        - Move TRAN-MERCHANT-NAME to MNAMEI
        - Move TRAN-MERCHANT-CITY to MCITYI
        - Move TRAN-MERCHANT-ZIP to MZIPI
   7. Perform PROCESS-ENTER-KEY to validate and display the copied data
   8. If an error occurs during any step, set the error flag, move an appropriate error message to WS-MESSAGE, and perform SEND-TRNADD-SCREEN
   9. If CDEMO-TO-PROGRAM is LOW-VALUES or SPACES, set it to 'COSGN00C'
   10. Move WS-TRANID to CDEMO-FROM-TRANID
   11. Move WS-PGMNAME to CDEMO-FROM-PROGRAM
   12. Move ZEROS to CDEMO-PGM-CONTEXT
   13. Execute CICS XCTL to the program specified in CDEMO-TO-PROGRAM with CARDDEMO-COMMAREA

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Read CXACAIX File:
   - Detailed Description of the Rule: This function reads the CXACAIX file to retrieve the card number associated with an account ID.
   - What it proposes to do: Look up the card number for a given account ID in the cross-reference file.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Perform CICS READ operation:
      - Use WS-CXACAIX-FILE as the dataset
      - Read into CARD-XREF-RECORD
      - Use XREF-ACCT-ID as the key
      - Set KEYLENGTH to the length of XREF-ACCT-ID
   2. Handle the response:
      - If RESP is DFHRESP(NORMAL):
        - Continue processing
      - If RESP is DFHRESP(NOTFND):
        - Set error flag (WS-ERR-FLG) to 'Y'
        - Set error message (WS-MESSAGE) to 'Account ID NOT found...'
        - Set cursor position to ACTIDINL of COTRN2AI
        - Perform SEND-TRNADD-SCREEN
      - For any other response:
        - Display RESP and REAS codes
        - Set error flag (WS-ERR-FLG) to 'Y'
        - Set error message (WS-MESSAGE) to 'Unable to lookup Acct in XREF AIX file...'
        - Set cursor position to ACTIDINL of COTRN2AI
        - Perform SEND-TRNADD-SCREEN
   3. If successful, move XREF-CARD-NUM to CARDNINI of COTRN2AI
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 6
Here's the revised business rule based on the COBOL program:

```markdown
- ## Rule / Function / Method - Read CCXREF File:
   - Detailed Description of the Rule: This function reads the CCXREF file to retrieve the account ID associated with a card number.
   - What it proposes to do: Look up the account ID for a given card number in the cross-reference file.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Perform CICS READ operation:
      - Use WS-CCXREF-FILE as the dataset
      - Read into CARD-XREF-RECORD
      - Use XREF-CARD-NUM as the key
      - Set KEYLENGTH to the length of XREF-CARD-NUM
   2. Handle the response:
      - If RESP is DFHRESP(NORMAL):
        - Continue processing
      - If RESP is DFHRESP(NOTFND):
        - Set error flag (WS-ERR-FLG) to 'Y'
        - Set error message to 'Card Number NOT found...'
        - Set cursor position to CARDNINL of COTRN2AI
        - Perform SEND-TRNADD-SCREEN
      - For any other response:
        - Display RESP and REAS codes
        - Set error flag (WS-ERR-FLG) to 'Y'
        - Set error message to 'Unable to lookup Card # in XREF file...'
        - Set cursor position to CARDNINL of COTRN2AI
        - Perform SEND-TRNADD-SCREEN
   3. If successful, move XREF-ACCT-ID to ACTIDINI of COTRN2AI
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 7
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - Write TRANSACT File:
   - Detailed Description of the Rule: This function writes a new transaction record to the TRANSACT file.
   - What it proposes to do: Add a new transaction record to the main transaction file.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Validate input key fields:
      - Either Account ID or Card Number must be entered
      - If Account ID is entered:
        - Verify it's numeric
        - Read CXACAIX file to get corresponding Card Number
      - If Card Number is entered:
        - Verify it's numeric
        - Read CCXREF file to get corresponding Account ID
   2. Validate input data fields:
      - Ensure all required fields are not empty: Type CD, Category CD, Source, Description, Amount, Orig Date, Proc Date, Merchant ID, Merchant Name, Merchant City, Merchant ZIP
      - Verify Type CD and Category CD are numeric
      - Verify Amount is in format -99999999.99
      - Verify Orig Date and Proc Date are in format YYYY-MM-DD and are valid dates
      - Verify Merchant ID is numeric
   3. If 'Confirm' is 'Y':
      - Generate new Transaction ID:
        - Read the last record from TRANSACT file
        - Increment the last Transaction ID by 1
      - Prepare new TRAN-RECORD:
        - Set TRAN-ID to new Transaction ID
        - Copy all input fields to corresponding TRAN-RECORD fields
        - Convert Amount to internal numeric format
      - Perform CICS WRITE operation:
        - Use WS-TRANSACT-FILE as the dataset
        - Write from TRAN-RECORD
        - Use TRAN-ID as the key
   4. Handle the response:
      - If RESP is DFHRESP(NORMAL):
        - Perform INITIALIZE-ALL-FIELDS
        - Set success message with new Transaction ID
        - Perform SEND-TRNADD-SCREEN
      - If RESP is DFHRESP(DUPKEY) or DFHRESP(DUPREC):
        - Set error flag to 'Y'
        - Set error message to 'Tran ID already exist...'
        - Set cursor position to ACTIDINL
        - Perform SEND-TRNADD-SCREEN
      - For any other response:
        - Display RESP and REAS codes
        - Set error flag to 'Y'
        - Set error message to 'Unable to Add Transaction...'
        - Set cursor position to ACTIDINL
        - Perform SEND-TRNADD-SCREEN
   5. If 'Confirm' is 'N', space, or low-value:
      - Set error flag to 'Y'
      - Set error message to 'Confirm to add this transaction...'
      - Set cursor position to CONFIRML
      - Perform SEND-TRNADD-SCREEN
   6. If 'Confirm' is any other value:
      - Set error flag to 'Y'
      - Set error message to 'Invalid value. Valid values are (Y/N)...'
      - Set cursor position to CONFIRML
      - Perform SEND-TRNADD-SCREEN
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 8
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Initialize All Fields:
   - Detailed Description of the Rule: This function initializes all input fields on the screen to their default values.
   - What it proposes to do: Clear all input fields and prepare the screen for a new transaction entry.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Set cursor position (-1) to ACTIDINL
   2. Move SPACES to the following fields:
      - ACTIDINI (Account ID)
      - CARDNINI (Card Number)
      - TTYPCDI (Transaction Type Code)
      - TCATCDI (Transaction Category Code)
      - TRNSRCI (Transaction Source)
      - TRNAMTI (Transaction Amount)
      - TDESCI (Transaction Description)
      - TORIGDTI (Transaction Original Date)
      - TPROCDTI (Transaction Process Date)
      - MIDI (Merchant ID)
      - MNAMEI (Merchant Name)
      - MCITYI (Merchant City)
      - MZIPI (Merchant ZIP)
      - CONFIRMI (Confirmation)
   3. Clear WS-MESSAGE (error/success message field)
   4. Set ERR-FLG-OFF to TRUE (initialize error flag to 'N')
   5. Set USR-MODIFIED-NO to TRUE (initialize user modification flag to 'N')
   6. Move SPACES to ERRMSGO of COTRN2AO (clear error message output field)
```

<!--rule-end-->