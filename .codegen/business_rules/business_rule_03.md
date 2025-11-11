# CBTRN01C.cbl: Daily Transaction Processing Program

## Overview
This COBOL program, CBTRN01C, is part of the CardDemo application. It is a batch program designed to process daily transaction records. The main function of this program is to read records from a daily transaction file, verify the card numbers against a cross-reference file, and then update the corresponding account information.

The program plays a crucial role in maintaining up-to-date account information by processing daily transactions. It ensures that all transactions are properly recorded and associated with the correct customer accounts.

<!-- general-rule-start -->
## General Business Rules:

1. File Processing:
   - The program opens and processes several files: DALYTRAN (daily transactions), CUSTOMER, XREF (cross-reference), CARD, ACCOUNT, and TRANSACT.
   - It reads records from the DALYTRAN file sequentially.

2. Transaction Verification:
   - For each transaction record, the program verifies the card number using the XREF file.
   - If the card number is valid, it retrieves the corresponding account information from the ACCOUNT file.

3. Error Handling:
   - The program checks for file operation errors (opening, reading, closing) and displays appropriate error messages.
   - If a card number cannot be verified or an account is not found, the program skips the transaction and displays an error message.

4. Data Flow:
   - Daily transaction data is read from DALYTRAN-FILE.
   - Card numbers are verified against XREF-FILE.
   - Account information is retrieved from ACCOUNT-FILE.
   - Customer data can be accessed from CUSTOMER-FILE if needed.

5. Processing Logic:
   - The program continues processing until it reaches the end of the DALYTRAN file.
   - For each valid transaction, it displays the transaction record, card number, account ID, and customer ID.

6. File Closure:
   - After processing all transactions, the program properly closes all opened files.

7. Error Reporting:
   - The program uses a custom routine (Z-DISPLAY-IO-STATUS) to display detailed file status information in case of errors.

8. Program Termination:
   - In case of critical errors, the program can terminate itself using the Z-ABEND-PROGRAM routine.

This program is essential for maintaining the integrity and accuracy of the card transaction system by ensuring that daily transactions are properly processed and associated with the correct accounts and customers.
<!-- general-rule-end -->
## Dependencies

This program relies on several external files and copybooks for its operation. These dependencies are crucial for processing daily transactions, verifying card information, and updating account details. Understanding these dependencies is essential for any modernization efforts, as they represent the core data structures and file interactions of the system.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| DALYTRAN | Daily transaction file | File | DALYTRAN | Critical for input of daily transactions. May need to be replaced with a more modern data ingestion method. |
| CUSTFILE | Customer information file | Indexed File | CUSTFILE | Contains customer data. Could be replaced with a relational database table. |
| XREFFILE | Cross-reference file for card numbers | Indexed File | XREFFILE | Links card numbers to accounts. Could be integrated into a relational database structure. |
| CARDFILE | Card information file | Indexed File | CARDFILE | Stores card details. Potential candidate for database migration. |
| ACCTFILE | Account information file | Indexed File | ACCTFILE | Holds account data. Prime candidate for database migration. |
| TRANFILE | Transaction file | Indexed File | TRANFILE | Stores processed transactions. Could be replaced with a transaction log in a database. |
| CVTRA06Y | Copybook for daily transaction record structure | Copybook | CVTRA06Y | Defines data structure. May need updating if transaction format changes. |
| CVCUS01Y | Copybook for customer record structure | Copybook | CVCUS01Y | Defines customer data structure. Important for data migration planning. |
| CVACT03Y | Copybook for card cross-reference record structure | Copybook | CVACT03Y | Defines card-account-customer relationship. Crucial for system redesign. |
| CVACT02Y | Copybook for card record structure | Copybook | CVACT02Y | Defines card data structure. Important for modernizing card management. |
| CVACT01Y | Copybook for account record structure | Copybook | CVACT01Y | Defines account data structure. Key for updating account management systems. |
| CVTRA05Y | Copybook for transaction record structure | Copybook | CVTRA05Y | Defines transaction data structure. May need revision for modern transaction processing. |

These dependencies represent the core data structures and file interactions of the system. In a modernization effort, these files and copybooks would likely be replaced or transformed into more modern data storage and access methods, such as relational databases or APIs. The copybooks, in particular, provide valuable information about the current data structures, which will be crucial for data migration and system redesign.
## Detailed Rules
## Data Structure

This section describes the key data structures used in the CBTRN01C program. These structures are crucial for processing daily transactions and managing customer, account, and card information.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| 1 | DALYTRAN-ID | Alphanumeric | X(16) | Daily transaction ID | Yes | None | Unique identifier for each transaction | Yes | Transaction |
| 2 | DALYTRAN-TYPE-CD | Alphanumeric | X(02) | Transaction type code | Yes | None | Indicates the type of transaction | Yes | Transaction |
| 3 | DALYTRAN-CAT-CD | Numeric | 9(04) | Transaction category code | Yes | None | Categorizes the transaction | Yes | Transaction |
| 4 | DALYTRAN-SOURCE | Alphanumeric | X(10) | Transaction source | Yes | None | Indicates where the transaction originated | Yes | Transaction |
| 5 | DALYTRAN-DESC | Alphanumeric | X(100) | Transaction description | Yes | None | Detailed description of the transaction | Yes | Transaction |
| 6 | DALYTRAN-AMT | Numeric | S9(09)V99 | Transaction amount | Yes | None | Amount of the transaction | Yes | Transaction |
| 7 | DALYTRAN-MERCHANT-ID | Numeric | 9(09) | Merchant ID | Yes | None | Identifier for the merchant | Yes | Transaction |
| 8 | DALYTRAN-MERCHANT-NAME | Alphanumeric | X(50) | Merchant name | Yes | None | Name of the merchant | Yes | Transaction |
| 9 | DALYTRAN-MERCHANT-CITY | Alphanumeric | X(50) | Merchant city | Yes | None | City where the merchant is located | Yes | Transaction |
| 10 | DALYTRAN-MERCHANT-ZIP | Alphanumeric | X(10) | Merchant ZIP code | Yes | None | ZIP code of the merchant | Yes | Transaction |
| 11 | DALYTRAN-CARD-NUM | Alphanumeric | X(16) | Card number | Yes | None | Number of the card used in the transaction | Yes | Transaction |
| 12 | DALYTRAN-ORIG-TS | Alphanumeric | X(26) | Original timestamp | Yes | None | Timestamp when the transaction occurred | Yes | Transaction |
| 13 | DALYTRAN-PROC-TS | Alphanumeric | X(26) | Processing timestamp | Yes | None | Timestamp when the transaction was processed | Yes | Transaction |
| 14 | CUST-ID | Numeric | 9(09) | Customer ID | Yes | None | Unique identifier for each customer | Yes | Customer |
| 15 | CUST-FIRST-NAME | Alphanumeric | X(25) | Customer first name | Yes | None | First name of the customer | Yes | Customer |
| 16 | CUST-MIDDLE-NAME | Alphanumeric | X(25) | Customer middle name | No | None | Middle name of the customer | Yes | Customer |
| 17 | CUST-LAST-NAME | Alphanumeric | X(25) | Customer last name | Yes | None | Last name of the customer | Yes | Customer |
| 18 | CUST-SSN | Numeric | 9(09) | Customer SSN | Yes | None | Social Security Number of the customer | Yes | Customer |
| 19 | XREF-CARD-NUM | Alphanumeric | X(16) | Card number in cross-reference | Yes | None | Card number used for cross-referencing | Yes | Cross-reference |
| 20 | XREF-CUST-ID | Numeric | 9(09) | Customer ID in cross-reference | Yes | None | Customer ID linked to the card | Yes | Cross-reference |
| 21 | XREF-ACCT-ID | Numeric | 9(11) | Account ID in cross-reference | Yes | None | Account ID linked to the card | Yes | Cross-reference |
| 22 | CARD-NUM | Alphanumeric | X(16) | Card number | Yes | None | Unique identifier for each card | Yes | Card |
| 23 | CARD-ACCT-ID | Numeric | 9(11) | Account ID linked to card | Yes | None | Account associated with the card | Yes | Card |
| 24 | CARD-CVV-CD | Numeric | 9(03) | Card CVV code | Yes | None | Security code for the card | Yes | Card |
| 25 | CARD-EXPIRAION-DATE | Alphanumeric | X(10) | Card expiration date | Yes | None | Date when the card expires | Yes | Card |
| 26 | ACCT-ID | Numeric | 9(11) | Account ID | Yes | None | Unique identifier for each account | Yes | Account |
| 27 | ACCT-ACTIVE-STATUS | Alphanumeric | X(01) | Account active status | Yes | None | Indicates if the account is active | Yes | Account |
| 28 | ACCT-CURR-BAL | Numeric | S9(10)V99 | Account current balance | Yes | None | Current balance of the account | Yes | Account |
| 29 | ACCT-CREDIT-LIMIT | Numeric | S9(10)V99 | Account credit limit | Yes | None | Credit limit of the account | Yes | Account |
| 30 | TRAN-ID | Alphanumeric | X(16) | Transaction ID | Yes | None | Unique identifier for each transaction | Yes | Transaction |

This data structure is crucial for the modernization process as it defines the core entities (Customer, Account, Card, Transaction) and their relationships in the current system. During modernization, these structures may need to be adapted to fit modern database schemas or API designs while maintaining the essential business logic and data relationships.



<!--rule-start-->
## Reviewed Rule 1
```markdown
- ## Rule / Function / Method - Name: Daily Transaction Processing

- Detailed Description of the Rule:
  This rule governs the main process of reading daily transactions, verifying card numbers, and updating account information.
- What it proposes to do:
  Process each transaction in the daily transaction file, ensuring it's associated with a valid card and account.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Open all required files: DALYTRAN, CUSTOMER, XREF, CARD, ACCOUNT, and TRANSACT.
2. Initialize END-OF-DAILY-TRANS-FILE to 'N'.
3. While END-OF-DAILY-TRANS-FILE is 'N':
   a. Read the next record from DALYTRAN-FILE into DALYTRAN-RECORD.
   b. If read is successful (DALYTRAN-STATUS = '00'):
      i. Display DALYTRAN-RECORD.
      ii. Set WS-XREF-READ-STATUS to 0.
      iii. Move DALYTRAN-CARD-NUM to XREF-CARD-NUM.
      iv. Perform LOOKUP-XREF:
          - Read XREF-FILE using XREF-CARD-NUM as key.
          - If read is successful:
            * Display "SUCCESSFUL READ OF XREF".
            * Display CARD NUMBER, ACCOUNT ID, and CUSTOMER ID.
          - If read fails:
            * Display "INVALID CARD NUMBER FOR XREF".
            * Set WS-XREF-READ-STATUS to 4.
      v. If WS-XREF-READ-STATUS is 0:
         - Set WS-ACCT-READ-STATUS to 0.
         - Move XREF-ACCT-ID to ACCT-ID.
         - Perform READ-ACCOUNT:
           * Read ACCOUNT-FILE using ACCT-ID as key.
           * If read is successful:
             - Display "SUCCESSFUL READ OF ACCOUNT FILE".
           * If read fails:
             - Display "INVALID ACCOUNT NUMBER FOUND".
             - Set WS-ACCT-READ-STATUS to 4.
         - If WS-ACCT-READ-STATUS is not 0:
           * Display "ACCOUNT [ACCT-ID] NOT FOUND".
      vi. If WS-XREF-READ-STATUS is not 0:
          - Display "CARD NUMBER [DALYTRAN-CARD-NUM] COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-[DALYTRAN-ID]".
   c. If read results in end-of-file (DALYTRAN-STATUS = '10'):
      i. Set END-OF-DAILY-TRANS-FILE to 'Y'.
   d. If read results in any other status:
      i. Display "ERROR READING DAILY TRANSACTION FILE".
      ii. Display the IO status.
      iii. Abort the program.
4. After processing all records, close all opened files in this order: DALYTRAN, CUSTOMER, XREF, CARD, ACCOUNT, and TRANSACT.
5. Display "END OF EXECUTION OF PROGRAM CBTRN01C".

## Additional Details:
- The program uses specific file statuses for error handling: '00' for success, '10' for end-of-file, and any other status for errors.
- Each file open and close operation is performed separately with error checking.
- The program uses APPL-RESULT variable to track operation results, with 0 indicating success, 16 indicating end-of-file, and other values for errors.
- If any file operation fails during opening, reading, or closing, the program displays an error message, shows the IO status, and abends with code 999.
- The Z-DISPLAY-IO-STATUS paragraph is used to format and display file status codes in a user-friendly manner.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
- ## Rule / Function / Method - Name: File Opening

- Detailed Description of the Rule:
  This rule defines the process of opening each required file at the start of the program.
- What it proposes to do:
  Ensure all necessary files are opened correctly before processing begins.

## Rule Status: 
- Relevant for Modernization

## Algorithm
For each file (DALYTRAN, CUSTOMER, XREF, CARD, ACCOUNT, TRANSACT):
1. Set APPL-RESULT to 8.
2. Open the file in INPUT mode.
3. Check the file status:
   - If status is '00' (successful open):
     * Set APPL-RESULT to 0.
   - Else:
     * Set APPL-RESULT to 12.
4. If APPL-RESULT is not 0:
   - Display "ERROR OPENING [FILE NAME] FILE".
   - Display the IO status.
   - Abort the program.
<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
```markdown
- ## Rule / Function / Method - Name: File Closing

- Detailed Description of the Rule:
  This rule defines the process of closing each opened file at the end of the program.
- What it proposes to do:
  Ensure all opened files are properly closed before program termination.

## Rule Status: 
- Relevant for Modernization

## Algorithm
For each file (DALYTRAN, CUSTOMER, XREF, CARD, ACCOUNT, TRANSACT):
1. Set APPL-RESULT to 8 by adding 8 to ZERO.
2. Close the file.
3. Check the file status:
   - If status is '00' (successful close):
     * Set APPL-RESULT to 0.
   - Else:
     * Set APPL-RESULT to 12.
4. If APPL-RESULT is not 0 (not APPL-AOK):
   - Display "ERROR CLOSING [FILE NAME] FILE".
   - Move the specific file status (e.g., CUSTFILE-STATUS, XREFFILE-STATUS) to IO-STATUS.
   - Perform Z-DISPLAY-IO-STATUS to show detailed file status information.
   - Perform Z-ABEND-PROGRAM to abort the program.
5. If APPL-RESULT is 0 (APPL-AOK), continue to the next file.

Additional Details:
- The Z-DISPLAY-IO-STATUS procedure provides a detailed display of the file status, including handling for non-numeric statuses and special cases.
- The Z-ABEND-PROGRAM procedure sets TIMING to 0, ABCODE to 999, and calls 'CEE3ABD' to abort the program.
- Each file has its own specific close procedure (e.g., 9000-DALYTRAN-CLOSE, 9100-CUSTFILE-CLOSE) following this algorithm.
- The order of file closing is: DALYTRAN, CUSTOMER, XREF, CARD, ACCOUNT, TRANSACT.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
```markdown
- ## Rule / Function / Method - Name: LOOKUP-XREF

- Detailed Description of the Rule:
  This rule defines the process of looking up a card number in the cross-reference file.
- What it proposes to do:
  Verify if a card number is valid and retrieve associated account and customer information.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Move XREF-CARD-NUM to FD-XREF-CARD-NUM.
2. Read XREF-FILE record into CARD-XREF-RECORD using FD-XREF-CARD-NUM as the key.
3. If the read is successful (NOT INVALID KEY):
   - Display "SUCCESSFUL READ OF XREF".
   - Display "CARD NUMBER: [XREF-CARD-NUM]".
   - Display "ACCOUNT ID : [XREF-ACCT-ID]".
   - Display "CUSTOMER ID: [XREF-CUST-ID]".
   - Set WS-XREF-READ-STATUS to 0.
4. If the read fails (INVALID KEY):
   - Display "INVALID CARD NUMBER FOR XREF".
   - Set WS-XREF-READ-STATUS to 4.
5. The WS-XREF-READ-STATUS is used in subsequent processing to determine if the cross-reference lookup was successful.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
```markdown
- ## Rule / Function / Method - Name: READ-ACCOUNT

- Detailed Description of the Rule:
  This rule defines the process of reading an account record from the account file.
- What it proposes to do:
  Retrieve account information for a given account ID.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Move ACCT-ID to FD-ACCT-ID.
2. Read ACCOUNT-FILE record into ACCOUNT-RECORD using FD-ACCT-ID as the key.
3. If the read is successful (NOT INVALID KEY):
   - Display "SUCCESSFUL READ OF ACCOUNT FILE".
4. If the read fails (INVALID KEY):
   - Display "INVALID ACCOUNT NUMBER FOUND".
   - Set WS-ACCT-READ-STATUS to 4.
5. The read operation uses the ACCOUNT-FILE and reads the record into ACCOUNT-RECORD.
6. The key used for reading is FD-ACCT-ID, which is set from ACCT-ID before the read operation.
7. The status of the read operation is checked using the INVALID KEY and NOT INVALID KEY conditions.
8. WS-ACCT-READ-STATUS is used to track the success or failure of the read operation, with 0 indicating success and 4 indicating failure.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 6
```markdown
- ## Rule / Function / Method - Name: Z-DISPLAY-IO-STATUS

- Detailed Description of the Rule:
  This rule defines how to display detailed file status information.
- What it proposes to do:
  Provide a standardized way to output file operation status for debugging and error reporting.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. If IO-STATUS is not numeric OR IO-STAT1 is '9':
   a. Move IO-STAT1 to IO-STATUS-04(1:1).
   b. Move 0 to TWO-BYTES-BINARY.
   c. Move IO-STAT2 to TWO-BYTES-RIGHT.
   d. Move TWO-BYTES-BINARY to IO-STATUS-0403.
   e. Display "FILE STATUS IS: NNNN" followed by IO-STATUS-04.
2. Else:
   a. Move '0000' to IO-STATUS-04.
   b. Move IO-STATUS to IO-STATUS-04(3:2).
   c. Display "FILE STATUS IS: NNNN" followed by IO-STATUS-04.
3. Exit the procedure.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 7
```markdown
- ## Rule / Function / Method - Name: Z-ABEND-PROGRAM

- Detailed Description of the Rule:
  This rule defines how to abort the program in case of critical errors.
- What it proposes to do:
  Provide a standardized way to terminate the program abnormally.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Display the message "ABENDING PROGRAM".
2. Set TIMING to 0.
3. Set ABCODE to 999.
4. Call the 'CEE3ABD' function to abort the program.

## Additional Details:
- The TIMING and ABCODE variables are defined as PIC S9(9) BINARY.
- This procedure is called from various parts of the program when encountering critical errors, such as file I/O errors.
- The procedure does not take any parameters and does not return any values.
- After calling 'CEE3ABD', the program will terminate immediately without further execution.
```

<!--rule-end-->