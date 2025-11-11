# COTRN00C.cbl: Transaction List Screen

## Overview
This COBOL program, COTRN00C, is part of the CardDemo application and functions as a CICS transaction to list transactions from the TRANSACT file. It provides a user interface for viewing and navigating through transaction records, allowing users to select specific transactions for further details.

<!-- general-rule-start -->
## General Business Rules:
1. The program displays a list of transactions from the TRANSACT file, showing up to 10 transactions per page.
2. Users can navigate through the transaction list using PF7 (backward) and PF8 (forward) keys.
3. Each transaction entry displays the transaction ID, date, description, and amount.
4. Users can select a specific transaction for more details by entering 'S' next to the desired transaction.
5. The program supports pagination, keeping track of the current page number and whether there are more pages to display.
6. Input validation is performed to ensure that the transaction ID entered is numeric.
7. Error messages are displayed for invalid inputs or when system errors occur during file operations.
8. The program integrates with other modules in the CardDemo application, allowing navigation between different screens.
<!-- general-rule-end -->

Key functionalities include:
1. Displaying a paginated list of transactions
2. Handling user navigation through the transaction list
3. Processing user selection of specific transactions
4. Performing CRUD operations on the TRANSACT file
5. Input validation and error handling
6. Integration with other CardDemo modules

The program uses various CICS commands for screen handling (SEND, RECEIVE) and file operations (STARTBR, READNEXT, READPREV, ENDBR). It also maintains state information between screen interactions using a communication area (COMMAREA).

This program plays a crucial role in providing users with an overview of transactions and facilitating access to detailed transaction information within the CardDemo application.
## Dependencies

This program relies on several external dependencies for its functionality, including CICS commands, copybooks, and file systems. These dependencies are crucial for the program's operation and will need to be considered during any modernization efforts.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| CICS | Customer Information Control System, provides runtime environment and commands | Runtime Environment | N/A | High - May need to be replaced or emulated in a modernized system |
| COCOM01Y | Common communication area copybook | Copybook | COCOM01Y | Medium - Data structure may need to be adapted or replaced |
| COTRN00 | Screen map copybook for transaction list | Copybook | COTRN00 | High - UI layout may need to be redesigned for web or mobile interfaces |
| COTTL01Y | Title information copybook | Copybook | COTTL01Y | Low - May be replaced with configuration files or database |
| CSDAT01Y | Date handling copybook | Copybook | CSDAT01Y | Medium - Date handling may need to be updated for modern systems |
| CSMSG01Y | Message handling copybook | Copybook | CSMSG01Y | Medium - Message system may need to be redesigned |
| CVTRA05Y | Transaction record structure copybook | Copybook | CVTRA05Y | High - Core data structure, may need to be adapted for modern databases |
| DFHAID | CICS AID constants copybook | Copybook | DFHAID | Medium - May need to be replaced with equivalent modern key handling |
| DFHBMSCA | CICS BMS screen attribute constants | Copybook | DFHBMSCA | Medium - May need to be replaced with modern UI attribute handling |
| TRANSACT | Transaction file | File System | WS-TRANSACT-FILE | High - Data storage may need to be migrated to a modern database system |

These dependencies play crucial roles in the program's functionality, from defining data structures to handling user interface elements and file operations. During modernization, careful consideration must be given to how these dependencies will be handled, potentially replacing CICS-specific elements with more modern, platform-independent solutions.
## Detailed Rules
## Data Structure

This section describes the key data structures used in the COTRN00C program. The data structure is primarily based on the TRAN-RECORD, which represents a transaction in the system.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| 1 | TRAN-ID | Alphanumeric | X(16) | Unique identifier for the transaction | Yes | None | Primary key for TRANSACT file | High | Transaction |
| 2 | TRAN-TYPE-CD | Alphanumeric | X(02) | Code indicating the type of transaction | Yes | None | May need mapping to more descriptive values | Medium | Transaction |
| 3 | TRAN-CAT-CD | Numeric | 9(04) | Category code for the transaction | Yes | None | May need mapping to category descriptions | Medium | Transaction |
| 4 | TRAN-SOURCE | Alphanumeric | X(10) | Source of the transaction | Yes | None | Indicates origin of transaction | Medium | Transaction |
| 5 | TRAN-DESC | Alphanumeric | X(100) | Description of the transaction | Yes | None | Provides details about the transaction | High | Transaction |
| 6 | TRAN-AMT | Signed Numeric | S9(09)V99 | Amount of the transaction | Yes | None | Monetary value, includes 2 decimal places | High | Financial |
| 7 | TRAN-MERCHANT-ID | Numeric | 9(09) | Identifier for the merchant | Yes | None | Links to merchant information | Medium | Merchant |
| 8 | TRAN-MERCHANT-NAME | Alphanumeric | X(50) | Name of the merchant | Yes | None | Displayed on transaction list | Medium | Merchant |
| 9 | TRAN-MERCHANT-CITY | Alphanumeric | X(50) | City of the merchant | No | Spaces | Additional merchant information | Low | Merchant |
| 10 | TRAN-MERCHANT-ZIP | Alphanumeric | X(10) | ZIP code of the merchant | No | Spaces | Used for geographical analysis | Low | Merchant |
| 11 | TRAN-CARD-NUM | Alphanumeric | X(16) | Card number used for the transaction | Yes | None | Masked for security in displays | High | Card |
| 12 | TRAN-ORIG-TS | Alphanumeric | X(26) | Original timestamp of the transaction | Yes | None | Format: YYYY-MM-DD-HH.MM.SS.MMMMMM | High | Transaction |
| 13 | TRAN-PROC-TS | Alphanumeric | X(26) | Processing timestamp of the transaction | Yes | None | Format: YYYY-MM-DD-HH.MM.SS.MMMMMM | Medium | Transaction |
| 14 | WS-TRAN-DATE | Alphanumeric | X(08) | Formatted transaction date | No | '00/00/00' | Derived from TRAN-ORIG-TS for display | Medium | Display |
| 15 | WS-TRAN-AMT | Numeric Edited | +99999999.99 | Formatted transaction amount | No | None | Derived from TRAN-AMT for display | Medium | Display |
| 16 | CDEMO-CT00-TRNID-FIRST | Alphanumeric | X(16) | First transaction ID on current page | No | Spaces | Used for pagination | Medium | Navigation |
| 17 | CDEMO-CT00-TRNID-LAST | Alphanumeric | X(16) | Last transaction ID on current page | No | Spaces | Used for pagination | Medium | Navigation |
| 18 | CDEMO-CT00-PAGE-NUM | Numeric | 9(08) | Current page number | No | 0 | Used for pagination | Medium | Navigation |
| 19 | CDEMO-CT00-NEXT-PAGE-FLG | Alphanumeric | X(01) | Flag indicating if there's a next page | No | 'N' | 'Y' if more pages, 'N' if last page | Medium | Navigation |
| 20 | CDEMO-CT00-TRN-SEL-FLG | Alphanumeric | X(01) | Flag for selected transaction | No | Spaces | 'S' if a transaction is selected | Medium | User Input |
| 21 | CDEMO-CT00-TRN-SELECTED | Alphanumeric | X(16) | ID of the selected transaction | No | Spaces | Stores the selected transaction ID | Medium | User Input |

This data structure represents the core information handled by the COTRN00C program. It includes fields for transaction details, merchant information, and navigation controls. The structure is relevant for modernization efforts, especially in terms of data mapping to modern database systems and UI considerations for displaying transaction information.



<!--rule-start-->
## Reviewed Rule 1
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - PROCESS-ENTER-KEY:
    - Detailed Description of the Rule:
      This function processes the user's input when the Enter key is pressed on the transaction list screen.
    - What it proposes to do:
      It determines if a transaction has been selected for viewing, handles the navigation to the transaction detail screen, or processes a new transaction ID search.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. Check each selection field (SEL0001I to SEL0010I) to determine if a transaction has been selected.
    2. If a selection is found:
       a. Store the selection flag in CDEMO-CT00-TRN-SEL-FLG.
       b. Store the selected transaction ID in CDEMO-CT00-TRN-SELECTED.
    3. If a transaction is selected (CDEMO-CT00-TRN-SEL-FLG and CDEMO-CT00-TRN-SELECTED are not spaces and not low-values):
       a. If the selection flag is 'S' or 's':
          - Set CDEMO-TO-PROGRAM to 'COTRN01C'.
          - Set CDEMO-FROM-TRANID to the current transaction ID (WS-TRANID).
          - Set CDEMO-FROM-PROGRAM to the current program name (WS-PGMNAME).
          - Set CDEMO-PGM-CONTEXT to 0.
          - Transfer control to the COTRN01C program using EXEC CICS XCTL.
       b. If the selection flag is not 'S' or 's':
          - Set an error message: 'Invalid selection. Valid value is S'.
          - Set the cursor position to TRNIDINL.
    4. If no transaction is selected, check if a transaction ID has been entered in TRNIDINI:
       a. If TRNIDINI is spaces or low-values, set TRAN-ID to low-values.
       b. If TRNIDINI contains a value:
          - If TRNIDINI is numeric, move its value to TRAN-ID.
          - If TRNIDINI is not numeric:
            * Set the error flag (WS-ERR-FLG) to 'Y'.
            * Set an error message: 'Tran ID must be Numeric ...'.
            * Set the cursor position to TRNIDINL.
            * Call SEND-TRNLST-SCREEN to display the error.
    5. Set the cursor position to TRNIDINL.
    6. Set CDEMO-CT00-PAGE-NUM to 0.
    7. Call PROCESS-PAGE-FORWARD to load the first page of transactions:
       a. Perform STARTBR-TRANSACT-FILE to start browsing the TRANSACT file.
       b. If no error occurs:
          - Initialize transaction data for all 10 rows.
          - Read up to 10 transactions, populating the screen fields (TRNID01I to TRNID10I, TDATE01I to TDATE10I, etc.).
          - Set CDEMO-CT00-TRNID-FIRST to the first transaction ID and CDEMO-CT00-TRNID-LAST to the last transaction ID on the page.
          - Increment CDEMO-CT00-PAGE-NUM.
          - Check if there are more transactions (NEXT-PAGE-YES/NO).
       c. Perform ENDBR-TRANSACT-FILE to end browsing.
       d. Update PAGENUMI with the current page number.
    8. If no errors occurred, clear the TRNIDINO field on the screen.
    9. Call SEND-TRNLST-SCREEN to display the updated transaction list.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - PROCESS-PF7-KEY:
    - Detailed Description of the Rule:
      This function handles the user's request to view the previous page of transactions.
    - What it proposes to do:
      It navigates to the previous page of transactions if available.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. If CDEMO-CT00-TRNID-FIRST is spaces or low-values, set TRAN-ID to low-values.
       Otherwise, set TRAN-ID to CDEMO-CT00-TRNID-FIRST.
    2. Set NEXT-PAGE-YES flag to TRUE.
    3. Set the cursor position to TRNIDINL.
    4. If CDEMO-CT00-PAGE-NUM is greater than 1:
       a. Call PROCESS-PAGE-BACKWARD to load the previous page of transactions.
       b. PROCESS-PAGE-BACKWARD will:
          - Start browsing the TRANSACT file.
          - Initialize transaction data for all 10 rows.
          - Read previous records, populating up to 10 rows of transaction data.
          - Update CDEMO-CT00-PAGE-NUM.
          - End browsing the TRANSACT file.
          - Move CDEMO-CT00-PAGE-NUM to PAGENUMI of COTRN0AI.
          - Call SEND-TRNLST-SCREEN to display the updated transaction list.
    5. If CDEMO-CT00-PAGE-NUM is 1 or less:
       a. Set the message to 'You are already at the top of the page...'.
       b. Set SEND-ERASE-NO flag to TRUE.
       c. Call SEND-TRNLST-SCREEN to display the message without erasing the screen.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
Here's the revised business rule incorporating all the details from the COBOL program:

```markdown
- ## Rule / Function / Method - PROCESS-PF8-KEY:
    - Detailed Description of the Rule:
      This function handles the user's request to view the next page of transactions.
    - What it proposes to do:
      It navigates to the next page of transactions if available.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. If CDEMO-CT00-TRNID-LAST is spaces or low-values, set TRAN-ID to HIGH-VALUES.
       Otherwise, set TRAN-ID to CDEMO-CT00-TRNID-LAST.
    2. Set the cursor position to TRNIDINL.
    3. If NEXT-PAGE-YES flag is TRUE:
       a. Call PROCESS-PAGE-FORWARD to load the next page of transactions:
          - Perform STARTBR-TRANSACT-FILE to start browsing the TRANSACT file.
          - If not the first page (EIBAID not DFHENTER, DFHPF7, or DFHPF3), perform READNEXT-TRANSACT-FILE.
          - Initialize transaction data for all 10 rows.
          - Read up to 10 transactions, populating the screen fields (TRNID01I to TRNID10I, TDATE01I to TDATE10I, etc.).
          - Update CDEMO-CT00-PAGE-NUM, incrementing by 1.
          - Check for more transactions:
            - If found, set NEXT-PAGE-YES to TRUE.
            - If not found, set NEXT-PAGE-NO to TRUE.
          - Perform ENDBR-TRANSACT-FILE to end browsing.
          - Update PAGENUMI with the new page number.
          - Clear TRNIDINO field.
          - Call SEND-TRNLST-SCREEN to display the updated transaction list.
    4. If NEXT-PAGE-YES flag is FALSE:
       a. Set the message to 'You are already at the bottom of the page...'.
       b. Set SEND-ERASE-NO flag to TRUE.
       c. Call SEND-TRNLST-SCREEN to display the message.
    5. Update CDEMO-CT00-TRNID-FIRST with the ID of the first transaction on the new page.
    6. Update CDEMO-CT00-TRNID-LAST with the ID of the last transaction on the new page.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - PROCESS-PAGE-FORWARD:
    - Detailed Description of the Rule:
      This function retrieves and displays the next page of transactions.
    - What it proposes to do:
      It reads the next set of transactions from the TRANSACT file and populates the screen with the data.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. Call STARTBR-TRANSACT-FILE to start browsing the TRANSACT file.
    2. If no error occurred:
       a. If the action is not ENTER, PF7, or PF3, call READNEXT-TRANSACT-FILE.
       b. If there are more transactions and no error:
          - Initialize all 10 transaction data fields on the screen to spaces.
       c. Set WS-IDX to 1.
       d. Repeat until WS-IDX is 11, end of file is reached, or an error occurs:
          - Call READNEXT-TRANSACT-FILE to read the next transaction.
          - If a transaction is read successfully:
            * Call POPULATE-TRAN-DATA to add the transaction to the screen:
              - Format TRAN-AMT as +99999999.99
              - Format TRAN-ORIG-TS as MM/DD/YY
              - Populate fields based on WS-IDX (1-10):
                TRNID0xI, TDATE0xI, TDESC0xI, TAMT00xI
              - For WS-IDX 1, set CDEMO-CT00-TRNID-FIRST
              - For WS-IDX 10, set CDEMO-CT00-TRNID-LAST
            * Increment WS-IDX by 1.
       e. If there are more transactions and no error:
          - Increment CDEMO-CT00-PAGE-NUM by 1.
          - Call READNEXT-TRANSACT-FILE to check for more transactions.
          - If there are more transactions, set NEXT-PAGE-YES to TRUE.
          - Otherwise, set NEXT-PAGE-NO to TRUE.
       f. If end of file is reached:
          - Set NEXT-PAGE-NO to TRUE.
          - If WS-IDX is greater than 1, increment CDEMO-CT00-PAGE-NUM by 1.
       g. Call ENDBR-TRANSACT-FILE to end browsing the TRANSACT file.
       h. Move CDEMO-CT00-PAGE-NUM to PAGENUMI on the screen.
       i. Clear TRNIDINO on the screen.
       j. Call SEND-TRNLST-SCREEN to display the updated screen:
          - Populate header info with current date and time
          - Display any error messages in ERRMSGO
          - If SEND-ERASE-YES is TRUE, send with ERASE option
    3. If an error occurs at any point:
       - Set ERR-FLG-ON to TRUE
       - Set appropriate error message in WS-MESSAGE
       - Set cursor position to TRNIDINL
       - Call SEND-TRNLST-SCREEN to display the error
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - PROCESS-PAGE-BACKWARD:
    - Detailed Description of the Rule:
      This function retrieves and displays the previous page of transactions.
    - What it proposes to do:
      It reads the previous set of transactions from the TRANSACT file and populates the screen with the data.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. Call STARTBR-TRANSACT-FILE to start browsing the TRANSACT file.
    2. If no error occurred:
       a. If the action is not ENTER or PF8, call READPREV-TRANSACT-FILE.
       b. If there are more transactions and no error:
          - Initialize all 10 transaction data fields on the screen to spaces.
       c. Set WS-IDX to 10.
       d. Repeat until WS-IDX is 0, end of file is reached, or an error occurs:
          - Call READPREV-TRANSACT-FILE to read the previous transaction.
          - If a transaction is read successfully:
            * Call POPULATE-TRAN-DATA to add the transaction to the screen.
            * Decrement WS-IDX by 1.
       e. If there are more transactions and no error:
          - Call READPREV-TRANSACT-FILE to check for more transactions.
          - If NEXT-PAGE-YES is TRUE:
            * If there are more transactions and CDEMO-CT00-PAGE-NUM > 1:
              - Subtract 1 from CDEMO-CT00-PAGE-NUM.
            * Otherwise, set CDEMO-CT00-PAGE-NUM to 1.
       f. Call ENDBR-TRANSACT-FILE to end browsing the TRANSACT file.
       g. Move CDEMO-CT00-PAGE-NUM to PAGENUMI on the screen.
       h. Call SEND-TRNLST-SCREEN to display the updated screen.
    3. If an error occurs at any point:
       - Set ERR-FLG-ON to TRUE.
       - Set an appropriate error message in WS-MESSAGE.
       - Set the cursor position to TRNIDINL of COTRN0AI.
       - Call SEND-TRNLST-SCREEN to display the error message.
    4. Additional details:
       - The function uses TRAN-ID as the key for browsing the TRANSACT file.
       - If CDEMO-CT00-TRNID-FIRST is spaces or low-values, set TRAN-ID to low-values.
       - Otherwise, set TRAN-ID to CDEMO-CT00-TRNID-FIRST.
       - The function sets NEXT-PAGE-YES to TRUE at the beginning.
       - If CDEMO-CT00-PAGE-NUM is 1 or less, display a message "You are already at the top of the page..." and do not perform the backward browsing.
       - The POPULATE-TRAN-DATA subroutine formats the transaction date (MM/DD/YY) and amount for display.
       - The first transaction ID read is stored in CDEMO-CT00-TRNID-FIRST, and the last one in CDEMO-CT00-TRNID-LAST.
       - If no transactions are found or an error occurs, appropriate messages are displayed and the screen is refreshed.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 6
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - POPULATE-TRAN-DATA:
    - Detailed Description of the Rule:
      This function populates the screen with data from a single transaction record.
    - What it proposes to do:
      It formats and moves transaction data to the appropriate fields on the screen.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. Move TRAN-AMT to WS-TRAN-AMT.
    2. Move TRAN-ORIG-TS to WS-TIMESTAMP.
    3. Format the transaction date:
       a. Move WS-TIMESTAMP-DT-YYYY(3:2) to WS-CURDATE-YY.
       b. Move WS-TIMESTAMP-DT-MM to WS-CURDATE-MM.
       c. Move WS-TIMESTAMP-DT-DD to WS-CURDATE-DD.
       d. Move WS-CURDATE-MM-DD-YY to WS-TRAN-DATE.
    4. Based on the value of WS-IDX (1 to 10), populate the corresponding screen field:
       - Move TRAN-ID to TRNIDxxI (where xx is the index 01-10).
       - Move WS-TRAN-DATE to TDATExxI.
       - Move TRAN-DESC to TDESCxxI.
       - Move WS-TRAN-AMT to TAMTxxxI.
    5. If WS-IDX is 1, also move TRAN-ID to CDEMO-CT00-TRNID-FIRST.
    6. If WS-IDX is 10, also move TRAN-ID to CDEMO-CT00-TRNID-LAST.
    7. The specific field names for each index are:
       - Index 1: TRNID01I, TDATE01I, TDESC01I, TAMT001I
       - Index 2: TRNID02I, TDATE02I, TDESC02I, TAMT002I
       - Index 3: TRNID03I, TDATE03I, TDESC03I, TAMT003I
       - Index 4: TRNID04I, TDATE04I, TDESC04I, TAMT004I
       - Index 5: TRNID05I, TDATE05I, TDESC05I, TAMT005I
       - Index 6: TRNID06I, TDATE06I, TDESC06I, TAMT006I
       - Index 7: TRNID07I, TDATE07I, TDESC07I, TAMT007I
       - Index 8: TRNID08I, TDATE08I, TDESC08I, TAMT008I
       - Index 9: TRNID09I, TDATE09I, TDESC09I, TAMT009I
       - Index 10: TRNID10I, TDATE10I, TDESC10I, TAMT010I
    8. The function uses an EVALUATE statement to determine which set of fields to populate based on WS-IDX.
    9. All amount fields (TAMTxxxI) are formatted as +99999999.99.
    10. All date fields (TDATExxI) are formatted as MM/DD/YY.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 7
Here's the revised business rule incorporating all the details from the COBOL program:

```markdown
- ## Rule / Function / Method - STARTBR-TRANSACT-FILE:
    - Detailed Description of the Rule:
      This function initiates browsing of the TRANSACT file.
    - What it proposes to do:
      It sets up the file cursor for reading transactions sequentially.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. Execute CICS STARTBR command:
       - Set DATASET to WS-TRANSACT-FILE.
       - Set RIDFLD to TRAN-ID.
       - Set KEYLENGTH to the length of TRAN-ID.
       - Store response codes in WS-RESP-CD and WS-REAS-CD.
    2. Evaluate the response (WS-RESP-CD):
       a. If NORMAL:
          - Continue processing.
       b. If NOTFND:
          - Set TRANSACT-EOF to TRUE.
          - Set message to 'You are at the top of the page...'.
          - Set cursor position to TRNIDINL.
          - Call SEND-TRNLST-SCREEN.
       c. For any other response:
          - Display error information (RESP and REAS codes).
          - Set WS-ERR-FLG to 'Y'.
          - Set message to 'Unable to lookup transaction...'.
          - Set cursor position to TRNIDINL.
          - Call SEND-TRNLST-SCREEN.
    3. If not ERR-FLG-ON:
       - If EIBAID is not DFHENTER, DFHPF7, or DFHPF3:
         - Perform READNEXT-TRANSACT-FILE.
       - If TRANSACT-NOT-EOF and ERR-FLG-OFF:
         - Initialize transaction data for all 10 rows.
       - Process up to 10 transactions:
         - Perform READNEXT-TRANSACT-FILE.
         - If successful, populate transaction data.
       - Check for next page:
         - If more records exist, set NEXT-PAGE-YES.
         - Otherwise, set NEXT-PAGE-NO.
       - Update page number (CDEMO-CT00-PAGE-NUM).
       - Perform ENDBR-TRANSACT-FILE.
       - Display updated screen with SEND-TRNLST-SCREEN.
    4. Additional details:
       - TRAN-ID is used as the key for browsing.
       - The function handles pagination, showing up to 10 transactions per page.
       - It updates CDEMO-CT00-TRNID-FIRST and CDEMO-CT00-TRNID-LAST with the first and last transaction IDs on the page.
       - Transaction date is formatted as MM/DD/YY from the original timestamp.
       - Transaction amount is displayed with two decimal places.
       - The screen is populated with transaction ID, date, description, and amount for each row.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 8
Here's the revised business rule incorporating all the details from the COBOL program:

```markdown
- ## Rule / Function / Method - READNEXT-TRANSACT-FILE:
    - Detailed Description of the Rule:
      This function reads the next record from the TRANSACT file.
    - What it proposes to do:
      It retrieves the next transaction record in sequence.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. Execute CICS READNEXT command:
       - Set DATASET to WS-TRANSACT-FILE.
       - Set INTO to TRAN-RECORD.
       - Set LENGTH to the length of TRAN-RECORD.
       - Set RIDFLD to TRAN-ID.
       - Set KEYLENGTH to the length of TRAN-ID.
       - Store response codes in WS-RESP-CD and WS-REAS-CD.
    2. Evaluate the response (WS-RESP-CD):
       a. If NORMAL:
          - Continue processing.
       b. If ENDFILE:
          - Set TRANSACT-EOF to TRUE.
          - Set message to 'You have reached the bottom of the page...'.
          - Set cursor position to TRNIDINL of COTRN0AI.
          - Perform SEND-TRNLST-SCREEN.
       c. For any other response:
          - Display error information (RESP and REAS codes).
          - Set WS-ERR-FLG to 'Y'.
          - Set message to 'Unable to lookup transaction...'.
          - Set cursor position to TRNIDINL of COTRN0AI.
          - Perform SEND-TRNLST-SCREEN.
    3. If successful, populate transaction data:
       - Move TRAN-AMT to WS-TRAN-AMT.
       - Format TRAN-ORIG-TS into WS-TRAN-DATE (MM/DD/YY format).
       - Based on the current index (WS-IDX), populate the corresponding fields in COTRN0AI:
         - TRNID[xx]I with TRAN-ID
         - TDATE[xx]I with WS-TRAN-DATE
         - TDESC[xx]I with TRAN-DESC
         - TAMT0[xx]I with WS-TRAN-AMT
       - For the first record, also set CDEMO-CT00-TRNID-FIRST.
       - For the tenth record, also set CDEMO-CT00-TRNID-LAST.
    4. Increment WS-IDX by 1 after each successful read.
    5. Continue reading until 10 records are processed, TRANSACT-EOF is set, or an error occurs.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 9
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - READPREV-TRANSACT-FILE:
    - Detailed Description of the Rule:
      This function reads the previous record from the TRANSACT file.
    - What it proposes to do:
      It retrieves the previous transaction record in sequence.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. Execute CICS READPREV command:
       - Set DATASET to WS-TRANSACT-FILE.
       - Set INTO to TRAN-RECORD.
       - Set LENGTH to the length of TRAN-RECORD.
       - Set RIDFLD to TRAN-ID.
       - Set KEYLENGTH to the length of TRAN-ID.
       - Store response codes in WS-RESP-CD and WS-REAS-CD.
    2. Evaluate the response (WS-RESP-CD):
       a. If NORMAL:
          - Continue processing.
       b. If ENDFILE:
          - Set TRANSACT-EOF to TRUE.
          - Set message to 'You have reached the top of the page...'.
          - Set cursor position to TRNIDINL of COTRN0AI.
          - Perform SEND-TRNLST-SCREEN.
       c. For any other response:
          - Display error information (RESP and REAS codes).
          - Set WS-ERR-FLG to 'Y'.
          - Set message to 'Unable to lookup transaction...'.
          - Set cursor position to TRNIDINL of COTRN0AI.
          - Perform SEND-TRNLST-SCREEN.
    3. If successful (NORMAL response):
       - Update TRAN-ID with the key of the record just read.
       - This allows for subsequent READPREV operations to continue from this point.
    4. The function does not explicitly move data to output fields. This is handled in the calling routine (likely PROCESS-PAGE-BACKWARD).
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 10
Here's the revised business rule incorporating all the details from the COBOL program:

```markdown
- ## Rule / Function / Method - ENDBR-TRANSACT-FILE:
    - Detailed Description of the Rule:
      This function ends the browsing of the TRANSACT file.
    - What it proposes to do:
      It releases the file cursor and associated resources for the TRANSACT file.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. Execute CICS ENDBR command:
       - Set DATASET to WS-TRANSACT-FILE.
    2. No response code or reason code is checked after the ENDBR command.
    3. This function is called after completing a series of READNEXT or READPREV operations on the TRANSACT file.
    4. It is typically used in conjunction with STARTBR-TRANSACT-FILE to properly manage file browsing sessions.
    5. This function does not return any specific values or set any flags.
    6. It is a critical step in maintaining proper CICS file management and preventing resource leaks.
```

<!--rule-end-->