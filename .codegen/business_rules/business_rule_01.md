# COBIL00C.cbl: Bill Payment Processing

## Overview
This COBOL program, COBIL00C, is part of the CardDemo application and functions as a CICS COBOL program. Its primary purpose is to handle bill payments, specifically to pay the account balance in full and record a transaction for online bill payment. The program interacts with various files to retrieve account information, update balances, and record transactions.

<!-- general-rule-start -->
## General Business Rules:

1. Account Validation:
   - The program checks if the entered account ID is valid and exists in the system.
   - If the account ID is empty or not found, an error message is displayed.

2. Balance Check:
   - The program retrieves the current balance for the account.
   - If the current balance is zero or negative, the program informs the user that there is nothing to pay.

3. Payment Confirmation:
   - The user must confirm the payment by entering 'Y' or 'y'.
   - If the user enters 'N' or 'n', the screen is cleared, and no payment is processed.
   - If the confirmation field is left empty, the program proceeds with displaying the account information.

4. Payment Processing:
   - When the payment is confirmed, the program performs the following steps:
     a. Generates a new transaction ID by reading the last transaction and incrementing it.
     b. Creates a new transaction record with details such as transaction type, description, amount, card number, and timestamp.
     c. Updates the account balance by subtracting the payment amount.
     d. Writes the new transaction to the transaction file.
     e. Updates the account data file with the new balance.

5. Error Handling:
   - The program handles various error scenarios, such as invalid keys, file read/write errors, and displays appropriate error messages.

6. Screen Interaction:
   - The program populates the screen with header information, including current date and time.
   - It handles different function keys:
     - ENTER: Processes the entered data
     - PF3: Returns to the previous screen
     - PF4: Clears the current screen

7. Timestamp Generation:
   - The program generates a current timestamp for recording transactions.

8. File Operations:
   - The program interacts with multiple files:
     - ACCTDAT: Account data file
     - CXACAIX: Card cross-reference file
     - TRANSACT: Transaction file

9. Transaction Recording:
   - A new transaction is recorded with specific details:
     - Transaction type: '02'
     - Category code: 2
     - Source: 'POS TERM'
     - Description: 'BILL PAYMENT - ONLINE'
     - Merchant ID: 999999999
     - Merchant Name: 'BILL PAYMENT'
     - Merchant City and ZIP: 'N/A'

10. Success Message:
    - Upon successful payment processing, the program displays a success message with the new transaction ID.

This program serves as a crucial component in the online bill payment process, ensuring accurate account updates and transaction recording while providing a user-friendly interface for customers to pay their bills in full.
<!-- general-rule-end -->
## Dependencies

This COBOL program relies on several external dependencies for its operation. These dependencies include copybooks, which contain shared data structures, and VSAM files for data storage and retrieval. Understanding these dependencies is crucial for maintaining and potentially modernizing the application.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| COCOM01Y | Common communication area | Copybook | COCOM01Y | Contains shared data structures; may need to be converted to a modern data format |
| COBIL00 | Bill payment screen layout | Copybook | COBIL00 | Defines the user interface; may need to be replaced with a modern UI framework |
| COTTL01Y | Unknown (not visible in the provided code) | Copybook | COTTL01Y | May contain additional shared data; assess its content for modernization |
| CSDAT01Y | Unknown (not visible in the provided code) | Copybook | CSDAT01Y | May contain additional shared data; assess its content for modernization |
| CSMSG01Y | Unknown (not visible in the provided code) | Copybook | CSMSG01Y | May contain additional shared data; assess its content for modernization |
| CVACT01Y | Unknown (not visible in the provided code) | Copybook | CVACT01Y | May contain additional shared data; assess its content for modernization |
| CVACT03Y | Unknown (not visible in the provided code) | Copybook | CVACT03Y | May contain additional shared data; assess its content for modernization |
| CVTRA05Y | Unknown (not visible in the provided code) | Copybook | CVTRA05Y | May contain additional shared data; assess its content for modernization |
| DFHAID | CICS standard copybook for AID keys | Copybook | DFHAID | CICS-specific; may need to be replaced with platform-independent key handling |
| DFHBMSCA | CICS standard copybook for BMS screen attributes | Copybook | DFHBMSCA | CICS-specific; may need to be replaced with a modern UI framework |
| ACCTDAT | Account data file | VSAM File | WS-ACCTDAT-FILE | Contains account information; consider migrating to a relational database |
| CXACAIX | Card cross-reference file | VSAM File | WS-CXACAIX-FILE | Contains card-account relationships; consider migrating to a relational database |
| TRANSACT | Transaction file | VSAM File | WS-TRANSACT-FILE | Stores transaction records; consider migrating to a relational database or transaction log system |

These dependencies play a significant role in the program's functionality and data management. For modernization efforts, it's important to consider replacing VSAM files with modern database systems, updating the user interface to a web-based or graphical interface, and potentially restructuring the data stored in copybooks into more flexible formats like JSON or XML.
## Detailed Rules
## Data Structure

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| WS-PGMNAME | WS-PGMNAME | PIC X | 8 | Program name | Yes | 'COBIL00C' | Constant value | Yes | Internal |
| WS-TRANID | WS-TRANID | PIC X | 4 | Transaction ID | Yes | 'CB00' | Constant value | Yes | Internal |
| WS-MESSAGE | WS-MESSAGE | PIC X | 80 | Error or information message | No | SPACES | Used for displaying messages to the user | Yes | Internal |
| WS-TRANSACT-FILE | WS-TRANSACT-FILE | PIC X | 8 | Transaction file name | Yes | 'TRANSACT' | Constant value | Yes | Internal |
| WS-ACCTDAT-FILE | WS-ACCTDAT-FILE | PIC X | 8 | Account data file name | Yes | 'ACCTDAT ' | Constant value | Yes | Internal |
| WS-CXACAIX-FILE | WS-CXACAIX-FILE | PIC X | 8 | Card cross-reference file name | Yes | 'CXACAIX ' | Constant value | Yes | Internal |
| WS-ERR-FLG | WS-ERR-FLG | PIC X | 1 | Error flag | Yes | 'N' | 'Y' for error, 'N' for no error | Yes | Internal |
| WS-RESP-CD | WS-RESP-CD | PIC S9 | 9 | Response code | No | ZEROS | COMP | Yes | Internal |
| WS-REAS-CD | WS-REAS-CD | PIC S9 | 9 | Reason code | No | ZEROS | COMP | Yes | Internal |
| WS-USR-MODIFIED | WS-USR-MODIFIED | PIC X | 1 | User modification flag | Yes | 'N' | 'Y' if modified, 'N' if not | Yes | Internal |
| WS-CONF-PAY-FLG | WS-CONF-PAY-FLG | PIC X | 1 | Payment confirmation flag | Yes | 'N' | 'Y' for confirmed, 'N' for not confirmed | Yes | Internal |
| WS-TRAN-AMT | WS-TRAN-AMT | PIC +9 | 11 | Transaction amount | No | - | 2 decimal places | Yes | Financial |
| WS-CURR-BAL | WS-CURR-BAL | PIC +9 | 13 | Current balance | No | - | 2 decimal places | Yes | Financial |
| WS-TRAN-ID-NUM | WS-TRAN-ID-NUM | PIC 9 | 16 | Transaction ID number | No | ZEROS | Used for generating new transaction IDs | Yes | Internal |
| WS-TRAN-DATE | WS-TRAN-DATE | PIC X | 8 | Transaction date | No | '00/00/00' | Format: MM/DD/YY | Yes | Date |
| WS-ABS-TIME | WS-ABS-TIME | PIC S9 | 15 | Absolute time | No | 0 | COMP-3 | Yes | Internal |
| WS-CUR-DATE-X10 | WS-CUR-DATE-X10 | PIC X | 10 | Current date | No | SPACES | Format: YYYY-MM-DD | Yes | Date |
| WS-CUR-TIME-X08 | WS-CUR-TIME-X08 | PIC X | 8 | Current time | No | SPACES | Format: HH:MM:SS | Yes | Time |
| ACTIDINI | ACTIDINI | PIC X | 11 | Account ID input | Yes | - | From COBIL00 copybook | Yes | User Input |
| CURBALI | CURBALI | PIC X | 14 | Current balance display | No | - | From COBIL00 copybook | Yes | Display |
| CONFIRMI | CONFIRMI | PIC X | 1 | Confirmation input | Yes | - | From COBIL00 copybook | Yes | User Input |
| ACCT-ID | ACCT-ID | - | - | Account ID | Yes | - | From ACCOUNT-RECORD structure | Yes | Key |
| ACCT-CURR-BAL | ACCT-CURR-BAL | - | - | Account current balance | Yes | - | From ACCOUNT-RECORD structure | Yes | Financial |
| XREF-ACCT-ID | XREF-ACCT-ID | - | - | Cross-reference account ID | Yes | - | From CARD-XREF-RECORD structure | Yes | Key |
| XREF-CARD-NUM | XREF-CARD-NUM | - | - | Cross-reference card number | Yes | - | From CARD-XREF-RECORD structure | Yes | Financial |
| TRAN-ID | TRAN-ID | - | - | Transaction ID | Yes | - | From TRAN-RECORD structure | Yes | Key |
| TRAN-TYPE-CD | TRAN-TYPE-CD | - | - | Transaction type code | Yes | - | From TRAN-RECORD structure | Yes | Code |
| TRAN-CAT-CD | TRAN-CAT-CD | - | - | Transaction category code | Yes | - | From TRAN-RECORD structure | Yes | Code |
| TRAN-SOURCE | TRAN-SOURCE | - | - | Transaction source | Yes | - | From TRAN-RECORD structure | Yes | Descriptive |
| TRAN-DESC | TRAN-DESC | - | - | Transaction description | Yes | - | From TRAN-RECORD structure | Yes | Descriptive |
| TRAN-AMT | TRAN-AMT | - | - | Transaction amount | Yes | - | From TRAN-RECORD structure | Yes | Financial |
| TRAN-CARD-NUM | TRAN-CARD-NUM | - | - | Transaction card number | Yes | - | From TRAN-RECORD structure | Yes | Financial |
| TRAN-MERCHANT-ID | TRAN-MERCHANT-ID | - | - | Transaction merchant ID | Yes | - | From TRAN-RECORD structure | Yes | Identifier |
| TRAN-MERCHANT-NAME | TRAN-MERCHANT-NAME | - | - | Transaction merchant name | Yes | - | From TRAN-RECORD structure | Yes | Descriptive |
| TRAN-MERCHANT-CITY | TRAN-MERCHANT-CITY | - | - | Transaction merchant city | Yes | - | From TRAN-RECORD structure | Yes | Descriptive |
| TRAN-MERCHANT-ZIP | TRAN-MERCHANT-ZIP | - | - | Transaction merchant ZIP code | Yes | - | From TRAN-RECORD structure | Yes | Descriptive |
| TRAN-ORIG-TS | TRAN-ORIG-TS | - | - | Transaction origin timestamp | Yes | - | From TRAN-RECORD structure | Yes | Timestamp |
| TRAN-PROC-TS | TRAN-PROC-TS | - | - | Transaction process timestamp | Yes | - | From TRAN-RECORD structure | Yes | Timestamp |



<!--rule-start-->
## Reviewed Rule 1
```markdown
- ## Rule / Function / Method - Name: Process-Enter-Key

- Detailed Description of the Rule:
  This rule handles the processing of the ENTER key when pressed on the bill payment screen. It validates the input, confirms the payment, and processes the bill payment transaction.

- What it proposes to do:
  Validate user input, confirm payment intention, retrieve account information, process the payment, and update relevant records.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Initialize the confirmation flag (CONF-PAY-FLG) to 'N'.
2. Validate the Account ID input:
   - If ACTIDINI is empty or contains low-values:
     - Set error flag (WS-ERR-FLG) to 'Y'
     - Set error message to "Acct ID can NOT be empty..."
     - Set cursor position to ACTIDINL
     - Perform SEND-BILLPAY-SCREEN
   - Otherwise, continue to next step
3. If no error:
   - Move ACTIDINI to ACCT-ID and XREF-ACCT-ID
   - Check the confirmation input (CONFIRMI):
     - If 'Y' or 'y':
       - Set CONF-PAY-FLG to 'Y'
       - Perform READ-ACCTDAT-FILE
     - If 'N' or 'n':
       - Perform CLEAR-CURRENT-SCREEN
       - Set WS-ERR-FLG to 'Y'
     - If spaces or low-values:
       - Perform READ-ACCTDAT-FILE
     - For any other value:
       - Set WS-ERR-FLG to 'Y'
       - Set error message to "Invalid value. Valid values are (Y/N)..."
       - Set cursor position to CONFIRML
       - Perform SEND-BILLPAY-SCREEN
4. Move ACCT-CURR-BAL to WS-CURR-BAL and CURBALI
5. If no error and ACCT-CURR-BAL <= 0:
   - Set WS-ERR-FLG to 'Y'
   - Set error message to "You have nothing to pay..."
   - Set cursor position to ACTIDINL
   - Perform SEND-BILLPAY-SCREEN
6. If no error:
   - If CONF-PAY-YES:
     a. Perform READ-CXACAIX-FILE
     b. Set TRAN-ID to HIGH-VALUES
     c. Perform STARTBR-TRANSACT-FILE
     d. Perform READPREV-TRANSACT-FILE
     e. Perform ENDBR-TRANSACT-FILE
     f. Move TRAN-ID to WS-TRAN-ID-NUM
     g. Increment WS-TRAN-ID-NUM by 1
     h. Initialize TRAN-RECORD
     i. Populate TRAN-RECORD with new transaction details:
        - Set TRAN-ID to WS-TRAN-ID-NUM
        - Set TRAN-TYPE-CD to '02'
        - Set TRAN-CAT-CD to 2
        - Set TRAN-SOURCE to 'POS TERM'
        - Set TRAN-DESC to 'BILL PAYMENT - ONLINE'
        - Set TRAN-AMT to ACCT-CURR-BAL
        - Set TRAN-CARD-NUM to XREF-CARD-NUM
        - Set TRAN-MERCHANT-ID to 999999999
        - Set TRAN-MERCHANT-NAME to 'BILL PAYMENT'
        - Set TRAN-MERCHANT-CITY to 'N/A'
        - Set TRAN-MERCHANT-ZIP to 'N/A'
     j. Perform GET-CURRENT-TIMESTAMP
     k. Set TRAN-ORIG-TS and TRAN-PROC-TS to WS-TIMESTAMP
     l. Perform WRITE-TRANSACT-FILE
     m. Subtract TRAN-AMT from ACCT-CURR-BAL
     n. Perform UPDATE-ACCTDAT-FILE
     o. Initialize all fields
     p. Set WS-MESSAGE to a success message including the Transaction ID
     q. Set ERRMSGC to DFHGREEN
   - Else:
     - Set error message to "Confirm to make a bill payment..."
     - Set cursor position to CONFIRML
7. Perform SEND-BILLPAY-SCREEN
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
Here's the revised business rule incorporating all the details from the COBOL program:

```markdown
- ## Rule / Function / Method - Name: GET-CURRENT-TIMESTAMP

- Detailed Description of the Rule:
  This rule retrieves the current system time and formats it into a timestamp.

- What it proposes to do:
  Generate a formatted timestamp for use in transaction records.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Use CICS ASKTIME function to get the absolute time:
   - EXEC CICS ASKTIME ABSTIME(WS-ABS-TIME)
2. Use CICS FORMATTIME function to format the absolute time:
   - EXEC CICS FORMATTIME
     ABSTIME(WS-ABS-TIME)
     YYYYMMDD(WS-CUR-DATE-X10)
     DATESEP('-')
     TIME(WS-CUR-TIME-X08)
     TIMESEP(':')
3. Initialize WS-TIMESTAMP
4. Move WS-CUR-DATE-X10 to WS-TIMESTAMP(01:10)
5. Move WS-CUR-TIME-X08 to WS-TIMESTAMP(12:08)
6. Move ZEROS to WS-TIMESTAMP-TM-MS6

## Additional Details:
- WS-ABS-TIME is defined as PIC S9(15) COMP-3
- WS-CUR-DATE-X10 is defined as PIC X(10)
- WS-CUR-TIME-X08 is defined as PIC X(08)
- WS-TIMESTAMP is not explicitly defined in the given code, but it's used to store the final formatted timestamp
- The timestamp format is YYYY-MM-DD HH:MM:SS.mmmmmm, where the last 6 digits (microseconds) are set to zeros
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Name: READ-ACCTDAT-FILE

- Detailed Description of the Rule:
  This rule reads the account data file to retrieve account information and performs necessary actions based on the response.

- What it proposes to do:
  Fetch account details based on the provided account ID and handle various scenarios including successful retrieval, account not found, and other errors.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Execute CICS READ command:
   - Dataset: WS-ACCTDAT-FILE
   - Into: ACCOUNT-RECORD
   - Length: LENGTH OF ACCOUNT-RECORD
   - RIDFLD: ACCT-ID
   - KEYLENGTH: LENGTH OF ACCT-ID
   - UPDATE mode
2. Check the response code (WS-RESP-CD):
   - If DFHRESP(NORMAL):
     - Continue processing
   - If DFHRESP(NOTFND):
     - Set WS-ERR-FLG to 'Y'
     - Set error message to "Account ID NOT found..."
     - Set cursor position to ACTIDINL OF COBIL0AI
     - Perform SEND-BILLPAY-SCREEN
   - For any other response:
     - Display response and reason codes (DISPLAY 'RESP:' WS-RESP-CD 'REAS:' WS-REAS-CD)
     - Set WS-ERR-FLG to 'Y'
     - Set error message to "Unable to lookup Account..."
     - Set cursor position to ACTIDINL OF COBIL0AI
     - Perform SEND-BILLPAY-SCREEN
3. SEND-BILLPAY-SCREEN procedure:
   - Populate header information
   - Move WS-MESSAGE to ERRMSGO OF COBIL0AO
   - Send the COBIL0A map with the COBIL00 mapset, using COBIL0AO as the source, with ERASE and CURSOR options
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
```markdown
- ## Rule / Function / Method - Name: UPDATE-ACCTDAT-FILE

- Detailed Description of the Rule:
  This rule updates the account data file with the new balance after processing a payment.

- What it proposes to do:
  Update the account record with the new balance after a successful bill payment.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Execute CICS REWRITE command:
   - Dataset: WS-ACCTDAT-FILE
   - From: ACCOUNT-RECORD
   - Length: LENGTH OF ACCOUNT-RECORD
2. Check the response code (WS-RESP-CD):
   - If NORMAL:
     - Perform INITIALIZE-ALL-FIELDS
     - Clear WS-MESSAGE
     - Set ERRMSGC to DFHGREEN
     - Construct success message with transaction ID:
       "Payment successful. Your Transaction ID is [TRAN-ID]."
     - Perform SEND-BILLPAY-SCREEN
   - If NOTFND:
     - Set WS-ERR-FLG to 'Y'
     - Set error message to "Account ID NOT found..."
     - Set cursor position to ACTIDINL
     - Perform SEND-BILLPAY-SCREEN
   - For any other response:
     - Display response and reason codes
     - Set WS-ERR-FLG to 'Y'
     - Set error message to "Unable to Update Account..."
     - Set cursor position to ACTIDINL
     - Perform SEND-BILLPAY-SCREEN
3. If successful:
   - Reset ACTIDINL to -1
   - Clear ACTIDINI, CURBALI, CONFIRMI fields
   - Clear WS-MESSAGE
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
```markdown
- ## Rule / Function / Method - Name: READ-CXACAIX-FILE

- Detailed Description of the Rule:
  This rule reads the card cross-reference file to retrieve card information associated with the account.

- What it proposes to do:
  Fetch card details linked to the account for use in transaction recording.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Execute CICS READ command:
   - Dataset: WS-CXACAIX-FILE
   - Into: CARD-XREF-RECORD
   - Length: LENGTH OF CARD-XREF-RECORD
   - RIDFLD: XREF-ACCT-ID
   - KEYLENGTH: LENGTH OF XREF-ACCT-ID
2. Check the response code (WS-RESP-CD):
   - If NORMAL:
     - Continue processing
   - If NOTFND:
     - Set WS-ERR-FLG to 'Y'
     - Set error message to "Account ID NOT found..."
     - Set cursor position to ACTIDINL
     - Perform SEND-BILLPAY-SCREEN
   - For any other response:
     - Display response and reason codes
     - Set WS-ERR-FLG to 'Y'
     - Set error message to "Unable to lookup XREF AIX file..."
     - Set cursor position to ACTIDINL
     - Perform SEND-BILLPAY-SCREEN
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 6
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Name: WRITE-TRANSACT-FILE

- Detailed Description of the Rule:
  This rule writes a new transaction record to the transaction file for a bill payment and updates the account balance.

- What it proposes to do:
  Record the bill payment transaction in the transaction file and update the account balance.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Execute CICS WRITE command:
   - Dataset: WS-TRANSACT-FILE
   - From: TRAN-RECORD
   - Length: LENGTH OF TRAN-RECORD
   - RIDFLD: TRAN-ID
   - KEYLENGTH: LENGTH OF TRAN-ID
2. Check the response code (WS-RESP-CD):
   - If NORMAL:
     - Perform INITIALIZE-ALL-FIELDS
     - Clear WS-MESSAGE
     - Set ERRMSGC to DFHGREEN
     - Construct success message with transaction ID: "Payment successful. Your Transaction ID is [TRAN-ID]."
     - Perform SEND-BILLPAY-SCREEN
   - If DUPKEY or DUPREC:
     - Set WS-ERR-FLG to 'Y'
     - Set error message to "Tran ID already exist..."
     - Set cursor position to ACTIDINL
     - Perform SEND-BILLPAY-SCREEN
   - For any other response:
     - Display response and reason codes
     - Set WS-ERR-FLG to 'Y'
     - Set error message to "Unable to Add Bill pay Transaction..."
     - Set cursor position to ACTIDINL
     - Perform SEND-BILLPAY-SCREEN
3. Before writing the transaction:
   - Read the ACCTDAT file to get the current account balance
   - Read the CXACAIX file to get the card number
   - Generate a new transaction ID by reading the last transaction and incrementing it
   - Populate the TRAN-RECORD with the following details:
     - TRAN-TYPE-CD: '02'
     - TRAN-CAT-CD: 2
     - TRAN-SOURCE: 'POS TERM'
     - TRAN-DESC: 'BILL PAYMENT - ONLINE'
     - TRAN-AMT: Set to the current account balance (ACCT-CURR-BAL)
     - TRAN-CARD-NUM: Set from XREF-CARD-NUM
     - TRAN-MERCHANT-ID: 999999999
     - TRAN-MERCHANT-NAME: 'BILL PAYMENT'
     - TRAN-MERCHANT-CITY: 'N/A'
     - TRAN-MERCHANT-ZIP: 'N/A'
     - TRAN-ORIG-TS and TRAN-PROC-TS: Set to current timestamp
4. After successful write:
   - Update ACCTDAT file:
     - Subtract TRAN-AMT from ACCT-CURR-BAL
   - If update is successful, display success message
   - If update fails, display error message
5. Perform SEND-BILLPAY-SCREEN to show the result to the user
```

<!--rule-end-->