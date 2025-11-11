# CBACT04C.CBL: Interest Calculator Program

## Overview
CBACT04C.CBL is a COBOL batch program that calculates interest for credit card accounts. It processes transaction category balances, applies interest rates based on account groups, and generates interest transactions. The program updates account balances and creates new transaction records for the calculated interest.

<!-- general-rule-start -->
## General Business Rules:
1. The program reads transaction category balances from an input file (TCATBAL-FILE).
2. It processes each record, calculating interest for different transaction categories within each account.
3. The program uses a cross-reference file (XREF-FILE) to link account IDs with card numbers.
4. Interest rates are determined based on the account group, transaction type, and category, using the DISCGRP-FILE.
5. The program updates account balances in the ACCOUNT-FILE to reflect the calculated interest.
6. New transaction records are created for each interest calculation and written to the TRANSACT-FILE.
7. The program handles multiple accounts, processing all transaction categories for one account before moving to the next.
8. If a specific interest rate is not found for an account group, a default rate is used.
9. The program uses the current date (passed as a parameter) to generate unique transaction IDs for the interest transactions.
10. Error handling is implemented for file operations, with appropriate error messages and program termination in case of critical errors.
11. The program resets cycle credits and debits for each account after processing.
12. A running total of interest is maintained for each account and added to the current balance before moving to the next account.
<!-- general-rule-end -->

Key components of the program include:
1. File handling for multiple input and output files
2. Interest calculation based on transaction category balances and interest rates
3. Account balance updates
4. Generation of interest transaction records
5. Error handling and reporting

The program is designed to be run as part of a batch process, likely at the end of a billing cycle, to apply interest charges to credit card accounts based on their balances and applicable interest rates.
## Dependencies

This program relies on several external files for input and output operations. These dependencies are crucial for the program's functionality, as they provide the necessary data for interest calculations and store the results. Understanding these dependencies is important for any modernization efforts, as they represent the data flow and integration points of the system.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| TCATBAL-FILE | Transaction category balance file | Input File | TCATBALF | Critical - Contains balance data for interest calculations |
| XREF-FILE | Cross-reference file linking account IDs to card numbers | Input File | XREFFILE | Important - Provides mapping between accounts and cards |
| DISCGRP-FILE | Disclosure group file with interest rates | Input File | DISCGRP | Critical - Contains interest rate information |
| ACCOUNT-FILE | Account master file | I/O File | ACCTFILE | Critical - Stores account information and is updated with new balances |
| TRANSACT-FILE | Transaction output file | Output File | TRANSACT | Important - Stores generated interest transactions |
| CVTRA01Y | Copy file for transaction record layout | Copy Book | N/A | Important - Defines transaction record structure |
| CVACT03Y | Copy file for card cross-reference record layout | Copy Book | N/A | Important - Defines cross-reference record structure |
| CVTRA02Y | Copy file for disclosure group record layout | Copy Book | N/A | Important - Defines disclosure group record structure |
| CVACT01Y | Copy file for account record layout | Copy Book | N/A | Critical - Defines account record structure |
| CVTRA05Y | Copy file for transaction file record layout | Copy Book | N/A | Important - Defines output transaction record structure |

These dependencies represent the core data structures and file interactions of the program. In a modernization effort, these could be replaced with database tables, API calls, or more modern file formats, while maintaining the same logical data relationships.
## Detailed Rules
## Data Structure

This section describes the key data structures used in the CBACT04C program. These structures are crucial for understanding the data flow and processing logic of the interest calculation system.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| 1 | TRANCAT-ACCT-ID | Numeric | 11 | Account ID for the transaction category balance | Yes | None | Key field for identifying accounts | Yes | Account |
| 2 | TRANCAT-TYPE-CD | Alphanumeric | 2 | Transaction type code | Yes | None | Identifies the type of transaction | Yes | Transaction |
| 3 | TRANCAT-CD | Numeric | 4 | Transaction category code | Yes | None | Identifies the specific category of transaction | Yes | Transaction |
| 4 | TRAN-CAT-BAL | Numeric | Varies | Balance for the transaction category | Yes | None | Used for interest calculation | Yes | Financial |
| 5 | ACCT-ID | Numeric | 11 | Account identifier | Yes | None | Primary key for account records | Yes | Account |
| 6 | ACCT-CURR-BAL | Numeric | Varies | Current balance of the account | Yes | None | Updated with calculated interest | Yes | Financial |
| 7 | ACCT-CURR-CYC-CREDIT | Numeric | Varies | Current cycle credit amount | Yes | 0 | Reset after processing | Yes | Financial |
| 8 | ACCT-CURR-CYC-DEBIT | Numeric | Varies | Current cycle debit amount | Yes | 0 | Reset after processing | Yes | Financial |
| 9 | ACCT-GROUP-ID | Alphanumeric | 10 | Account group identifier | Yes | None | Used to determine interest rates | Yes | Account |
| 10 | XREF-CARD-NUM | Numeric | 16 | Credit card number | Yes | None | Linked to account for transaction recording | Yes | Account |
| 11 | DIS-INT-RATE | Numeric | Varies | Interest rate for the account group and transaction type | Yes | None | Used in interest calculation | Yes | Financial |
| 12 | TRAN-ID | Alphanumeric | 16 | Unique identifier for transactions | Yes | None | Generated for new interest transactions | Yes | Transaction |
| 13 | TRAN-AMT | Numeric | Varies | Amount of the transaction | Yes | None | Stores calculated interest amount | Yes | Financial |
| 14 | TRAN-ORIG-TS | Timestamp | 26 | Original timestamp of the transaction | Yes | Current timestamp | Format: YYYY-MM-DD-HH.MM.SS.NNNNNN | Yes | Metadata |
| 15 | TRAN-PROC-TS | Timestamp | 26 | Processing timestamp of the transaction | Yes | Current timestamp | Format: YYYY-MM-DD-HH.MM.SS.NNNNNN | Yes | Metadata |
| 16 | WS-TOTAL-INT | Numeric | Varies | Accumulator for total interest per account | No | 0 | Internal working storage field | Yes | Financial |
| 17 | WS-MONTHLY-INT | Numeric | Varies | Calculated monthly interest per transaction category | No | None | Internal working storage field | Yes | Financial |
| 18 | PARM-DATE | Alphanumeric | 10 | Date parameter passed to the program | Yes | None | Used in generating transaction IDs | Yes | Metadata |

This data structure represents the core elements used in the interest calculation process. The fields are sourced from various files and working storage areas within the program. Understanding these data elements is crucial for any modernization effort, as they represent the key information processed and generated by the system.



<!--rule-start-->
## Reviewed Rule 1
```markdown
- ## Rule / Function / Method - Name: Initialize and Open Files

- Detailed Description of the Rule:
  This rule initializes and opens all the necessary files for the program's operation.
- What it proposes to do:
  Ensure all required files are available and ready for reading or writing.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Open TCATBAL-FILE for input
   - If successful, set APPL-RESULT to 0
   - If unsuccessful, set APPL-RESULT to 12
   - If APPL-RESULT is not 0, display error message "ERROR OPENING TRANSACTION CATEGORY BALANCE", display IO status, and abort program
2. Open XREF-FILE for input
   - If successful, set APPL-RESULT to 0
   - If unsuccessful, set APPL-RESULT to 12
   - If APPL-RESULT is not 0, display error message "ERROR OPENING CROSS REF FILE", display IO status, and abort program
3. Open DISCGRP-FILE for input
   - If successful, set APPL-RESULT to 0
   - If unsuccessful, set APPL-RESULT to 12
   - If APPL-RESULT is not 0, display error message "ERROR OPENING DALY REJECTS FILE", display IO status, and abort program
4. Open ACCOUNT-FILE for input-output
   - If successful, set APPL-RESULT to 0
   - If unsuccessful, set APPL-RESULT to 12
   - If APPL-RESULT is not 0, display error message "ERROR OPENING ACCOUNT MASTER FILE", display IO status, and abort program
5. Open TRANSACT-FILE for output
   - If successful, set APPL-RESULT to 0
   - If unsuccessful, set APPL-RESULT to 12
   - If APPL-RESULT is not 0, display error message "ERROR OPENING TRANSACTION FILE", display IO status, and abort program
6. For all file operations:
   - Use file status codes '00' to indicate success
   - Display specific error messages for each file
   - Perform 9910-DISPLAY-IO-STATUS to show detailed file status
   - Perform 9999-ABEND-PROGRAM to terminate if any file fails to open
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - Name: Process Transaction Category Balances

- Detailed Description of the Rule:
  This rule reads and processes each record from the TCATBAL-FILE, calculating interest for each transaction category balance and updating account balances.
- What it proposes to do:
  Calculate interest for all transaction categories across all accounts, update account balances, and generate interest transactions.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Initialize WS-FIRST-TIME to 'Y'
2. Initialize WS-RECORD-COUNT to 0
3. Initialize WS-TRANID-SUFFIX to 0
4. Initialize END-OF-FILE to 'N'
5. Initialize WS-TOTAL-INT to 0
6. While END-OF-FILE is 'N':
   a. Read next record from TCATBAL-FILE into TRAN-CAT-BAL-RECORD
   b. If end of file reached, set END-OF-FILE to 'Y' and perform Update Account
   c. If not end of file:
      - Increment WS-RECORD-COUNT
      - If TRANCAT-ACCT-ID is different from WS-LAST-ACCT-NUM:
        - If WS-FIRST-TIME is not 'Y', perform Update Account
        - Set WS-FIRST-TIME to 'N'
        - Set WS-TOTAL-INT to 0
        - Move TRANCAT-ACCT-ID to WS-LAST-ACCT-NUM
        - Perform Get Account Data (read ACCOUNT-FILE)
        - Perform Get Cross-Reference Data (read XREF-FILE)
      - Perform Get Interest Rate:
        - Read DISCGRP-FILE using ACCT-GROUP-ID, TRANCAT-CD, and TRANCAT-TYPE-CD
        - If record not found, use 'DEFAULT' as ACCT-GROUP-ID and read again
      - If DIS-INT-RATE is not 0:
        - Perform Compute Interest:
          - Calculate WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200
          - Add WS-MONTHLY-INT to WS-TOTAL-INT
        - Write Transaction Record:
          - Generate TRAN-ID using PARM-DATE and WS-TRANID-SUFFIX
          - Set TRAN-TYPE-CD to '01'
          - Set TRAN-CAT-CD to '05'
          - Set TRAN-SOURCE to 'System'
          - Set TRAN-DESC to 'Int. for a/c ' concatenated with ACCT-ID
          - Set TRAN-AMT to WS-MONTHLY-INT
          - Set TRAN-MERCHANT-ID to 0
          - Set TRAN-MERCHANT-NAME, TRAN-MERCHANT-CITY, TRAN-MERCHANT-ZIP to spaces
          - Set TRAN-CARD-NUM to XREF-CARD-NUM
          - Set TRAN-ORIG-TS and TRAN-PROC-TS to current timestamp
        - Perform Compute Fees (to be implemented)
7. After loop ends, perform final Update Account
8. Update Account process:
   - Add WS-TOTAL-INT to ACCT-CURR-BAL
   - Set ACCT-CURR-CYC-CREDIT to 0
   - Set ACCT-CURR-CYC-DEBIT to 0
   - Rewrite ACCOUNT-RECORD to ACCOUNT-FILE
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
Here's the revised business rule incorporating all the details from the COBOL program:

```markdown
- ## Rule / Function / Method - Name: Get Account Data

- Detailed Description of the Rule:
  This rule retrieves the account data for the current account being processed and performs necessary validations and updates.
- What it proposes to do:
  Load the account information needed for interest calculations and updates, validate the data, and update the account balance if necessary.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Set FD-ACCT-ID to TRANCAT-ACCT-ID
2. Read ACCOUNT-FILE into ACCOUNT-RECORD using FD-ACCT-ID as the key
3. If record not found, display error message "ACCOUNT NOT FOUND: " followed by FD-ACCT-ID
4. If read is successful (ACCTFILE-STATUS is '00'):
   - Set APPL-RESULT to 0
5. Else:
   - Set APPL-RESULT to 12
   - Display error message "ERROR READING ACCOUNT FILE"
   - Display IO status
   - Abort program
6. If this is not the first time processing an account (WS-FIRST-TIME is 'N'):
   - Update the account balance:
     - Add WS-TOTAL-INT to ACCT-CURR-BAL
     - Set ACCT-CURR-CYC-CREDIT to 0
     - Set ACCT-CURR-CYC-DEBIT to 0
   - Rewrite the updated ACCOUNT-RECORD to ACCOUNT-FILE
   - If rewrite is unsuccessful:
     - Set APPL-RESULT to 12
     - Display error message "ERROR RE-WRITING ACCOUNT FILE"
     - Display IO status
     - Abort program
7. If this is the first time processing an account:
   - Set WS-FIRST-TIME to 'N'
8. Set WS-TOTAL-INT to 0
9. Set WS-LAST-ACCT-NUM to TRANCAT-ACCT-ID
10. Read XREF-FILE into CARD-XREF-RECORD using FD-XREF-ACCT-ID (set to TRANCAT-ACCT-ID) as the key
11. If XREF record not found, display error message "ACCOUNT NOT FOUND: " followed by FD-XREF-ACCT-ID
12. If read is unsuccessful (XREFFILE-STATUS is not '00'):
    - Set APPL-RESULT to 12
    - Display error message "ERROR READING XREF FILE"
    - Display IO status
    - Abort program
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
Here's the revised business rule incorporating all the details from the COBOL program:

```markdown
- ## Rule / Function / Method - Name: Get Cross-Reference Data

- Detailed Description of the Rule:
  This rule retrieves the cross-reference data linking the account ID to a card number.
- What it proposes to do:
  Obtain the card number associated with the current account for transaction recording.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Set FD-XREF-ACCT-ID to TRANCAT-ACCT-ID
2. Read XREF-FILE into CARD-XREF-RECORD using FD-XREF-ACCT-ID as the key
3. If record not found, display error message "ACCOUNT NOT FOUND: " followed by FD-XREF-ACCT-ID
4. If read is successful (XREFFILE-STATUS is '00'):
   - Set APPL-RESULT to 0
5. Else:
   - Set APPL-RESULT to 12
   - Display error message "ERROR READING XREF FILE"
   - Display IO status by performing 9910-DISPLAY-IO-STATUS
   - Abort program by performing 9999-ABEND-PROGRAM
6. If APPL-RESULT is 0 (APPL-AOK), continue processing
7. The CARD-XREF-RECORD now contains:
   - XREF-CARD-NUM (16 characters)
   - XREF-CUST-NUM (9 digits)
   - XREF-ACCT-ID (11 digits)
   - XREF-FILLER (14 characters)
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - Name: Get Interest Rate

- Detailed Description of the Rule:
  This rule retrieves the interest rate for the current transaction category and account group, calculates the interest, and updates the account balance.
- What it proposes to do:
  Determine the correct interest rate to apply for the current transaction category balance, calculate the interest, and create a transaction record for the interest.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Set FD-DIS-ACCT-GROUP-ID to ACCT-GROUP-ID from ACCOUNT-RECORD
2. Set FD-DIS-TRAN-CAT-CD to TRANCAT-CD from TRAN-CAT-BAL-RECORD
3. Set FD-DIS-TRAN-TYPE-CD to TRANCAT-TYPE-CD from TRAN-CAT-BAL-RECORD
4. Read DISCGRP-FILE into DIS-GROUP-RECORD using FD-DISCGRP-KEY
5. If record not found (DISCGRP-STATUS is '23'):
   - Display message "DISCLOSURE GROUP RECORD MISSING"
   - Display message "TRY WITH DEFAULT GROUP CODE"
   - Set FD-DIS-ACCT-GROUP-ID to 'DEFAULT'
   - Perform Get Default Interest Rate by reading DISCGRP-FILE again
6. If read is successful (DISCGRP-STATUS is '00' or '23'):
   - Set APPL-RESULT to 0
7. Else:
   - Set APPL-RESULT to 12
   - Display error message "ERROR READING DISCLOSURE GROUP FILE"
   - Display IO status
   - Abort program
8. If DIS-INT-RATE is not 0:
   - Compute monthly interest: WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200
   - Add WS-MONTHLY-INT to WS-TOTAL-INT
   - Create a new transaction record:
     - Generate TRAN-ID by concatenating PARM-DATE and WS-TRANID-SUFFIX
     - Set TRAN-TYPE-CD to '01'
     - Set TRAN-CAT-CD to '05'
     - Set TRAN-SOURCE to 'System'
     - Set TRAN-DESC to 'Int. for a/c ' concatenated with ACCT-ID
     - Set TRAN-AMT to WS-MONTHLY-INT
     - Set TRAN-MERCHANT-ID to 0
     - Set TRAN-MERCHANT-NAME, TRAN-MERCHANT-CITY, and TRAN-MERCHANT-ZIP to spaces
     - Set TRAN-CARD-NUM to XREF-CARD-NUM
     - Set TRAN-ORIG-TS and TRAN-PROC-TS to current timestamp in DB2 format
   - Write the new transaction record to TRANSACT-FILE
9. When processing for an account is complete:
   - Add WS-TOTAL-INT to ACCT-CURR-BAL
   - Set ACCT-CURR-CYC-CREDIT and ACCT-CURR-CYC-DEBIT to 0
   - Rewrite the updated ACCOUNT-RECORD to ACCOUNT-FILE
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 6
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - Name: Compute Interest

- Detailed Description of the Rule:
  This rule calculates the interest for each transaction category balance of an account, creates interest transactions, and updates the account balance.
- What it proposes to do:
  Determine the interest amount for each transaction category, create new interest transactions, and update the account balance.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Read transaction category balance records sequentially from TCATBAL-FILE.
2. For each new account encountered:
   a. If not the first account, perform step 7 for the previous account.
   b. Reset WS-TOTAL-INT to 0.
   c. Retrieve account data from ACCOUNT-FILE using TRANCAT-ACCT-ID.
   d. Retrieve card cross-reference data from XREF-FILE using TRANCAT-ACCT-ID.
3. For each transaction category balance record:
   a. Retrieve interest rate:
      - Use ACCT-GROUP-ID, TRANCAT-CD, and TRANCAT-TYPE-CD to read DISCGRP-FILE.
      - If record not found, use 'DEFAULT' as ACCT-GROUP-ID and retry.
   b. If DIS-INT-RATE is not 0, perform steps 4-5.
4. Calculate monthly interest:
   WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200
5. Add WS-MONTHLY-INT to WS-TOTAL-INT
6. Perform Write Transaction:
   a. Increment WS-TRANID-SUFFIX
   b. Generate TRAN-ID by concatenating PARM-DATE and WS-TRANID-SUFFIX
   c. Set TRAN-TYPE-CD to '01' (interest transaction)
   d. Set TRAN-CAT-CD to '05' (interest category)
   e. Set TRAN-SOURCE to 'System'
   f. Set TRAN-DESC to "Int. for a/c " concatenated with ACCT-ID
   g. Set TRAN-AMT to WS-MONTHLY-INT
   h. Set TRAN-MERCHANT-ID to 0
   i. Set TRAN-MERCHANT-NAME, TRAN-MERCHANT-CITY, and TRAN-MERCHANT-ZIP to spaces
   j. Set TRAN-CARD-NUM to XREF-CARD-NUM
   k. Get current timestamp and set TRAN-ORIG-TS and TRAN-PROC-TS
   l. Write TRAN-RECORD to TRANSACT-FILE
   m. If write is unsuccessful, display error and abort program
7. Update account record:
   a. Add WS-TOTAL-INT to ACCT-CURR-BAL
   b. Set ACCT-CURR-CYC-CREDIT to 0
   c. Set ACCT-CURR-CYC-DEBIT to 0
   d. Rewrite updated ACCOUNT-RECORD to ACCOUNT-FILE
8. Repeat steps 2-7 until end of TCATBAL-FILE is reached.
9. Perform step 7 for the last account processed.
10. Close all opened files (TCATBAL-FILE, XREF-FILE, DISCGRP-FILE, ACCOUNT-FILE, TRANSACT-FILE).

Note: The program uses various status checks throughout the process. If any file operation fails, it displays an error message, shows the file status, and aborts the program.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 7
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Name: Update Account

- Detailed Description of the Rule:
  This rule updates the account record with the total calculated interest, resets cycle amounts, and creates a transaction record for the interest.
- What it proposes to do:
  Apply the total interest to the account balance, reset cycle amounts, and generate an interest transaction.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Add WS-TOTAL-INT to ACCT-CURR-BAL
2. Set ACCT-CURR-CYC-CREDIT to 0
3. Set ACCT-CURR-CYC-DEBIT to 0
4. Rewrite FD-ACCTFILE-REC from ACCOUNT-RECORD
5. If rewrite is successful (ACCTFILE-STATUS is '00'):
   - Set APPL-RESULT to 0
6. Else:
   - Set APPL-RESULT to 12
   - Display error message "ERROR RE-WRITING ACCOUNT FILE"
   - Display IO status
   - Abort program
7. Generate a transaction record for the interest:
   - Create a unique TRAN-ID by concatenating PARM-DATE and WS-TRANID-SUFFIX
   - Set TRAN-TYPE-CD to '01'
   - Set TRAN-CAT-CD to '05'
   - Set TRAN-SOURCE to 'System'
   - Set TRAN-DESC to 'Int. for a/c ' concatenated with ACCT-ID
   - Set TRAN-AMT to WS-MONTHLY-INT
   - Set TRAN-MERCHANT-ID to 0
   - Set TRAN-MERCHANT-NAME to spaces
   - Set TRAN-MERCHANT-CITY to spaces
   - Set TRAN-MERCHANT-ZIP to spaces
   - Set TRAN-CARD-NUM to XREF-CARD-NUM
   - Set TRAN-ORIG-TS and TRAN-PROC-TS to the current timestamp in DB2 format
8. Write the transaction record to TRANSACT-FILE
9. If write is successful (TRANFILE-STATUS is '00'):
   - Set APPL-RESULT to 0
10. Else:
    - Set APPL-RESULT to 12
    - Display error message "ERROR WRITING TRANSACTION RECORD"
    - Display IO status
    - Abort program
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 8
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Name: Close Files

- Detailed Description of the Rule:
  This rule closes all opened files at the end of processing.
- What it proposes to do:
  Ensure proper closure of all files used by the program.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Close TCATBAL-FILE
   - If unsuccessful (TCATBALF-STATUS not '00'), display error message "ERROR CLOSING TRANSACTION BALANCE FILE"
   - Display the file status using the 9910-DISPLAY-IO-STATUS routine
   - Abort program by calling the 9999-ABEND-PROGRAM routine
2. Close XREF-FILE
   - If unsuccessful (XREFFILE-STATUS not '00'), display error message "ERROR CLOSING CROSS REF FILE"
   - Display the file status using the 9910-DISPLAY-IO-STATUS routine
   - Abort program by calling the 9999-ABEND-PROGRAM routine
3. Close DISCGRP-FILE
   - If unsuccessful (DISCGRP-STATUS not '00'), display error message "ERROR CLOSING DISCLOSURE GROUP FILE"
   - Display the file status using the 9910-DISPLAY-IO-STATUS routine
   - Abort program by calling the 9999-ABEND-PROGRAM routine
4. Close ACCOUNT-FILE
   - If unsuccessful (ACCTFILE-STATUS not '00'), display error message "ERROR CLOSING ACCOUNT FILE"
   - Display the file status using the 9910-DISPLAY-IO-STATUS routine
   - Abort program by calling the 9999-ABEND-PROGRAM routine
5. Close TRANSACT-FILE
   - If unsuccessful (TRANFILE-STATUS not '00'), display error message "ERROR CLOSING TRANSACTION FILE"
   - Display the file status using the 9910-DISPLAY-IO-STATUS routine
   - Abort program by calling the 9999-ABEND-PROGRAM routine

Note: For each file closure, if successful (status '00'), the APPL-RESULT is set to 0. If unsuccessful, APPL-RESULT is set to 12 before the error handling procedures.
```

<!--rule-end-->