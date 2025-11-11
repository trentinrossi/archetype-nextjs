# CORPT00C.CBL: Transaction Report Generation Program

## Overview
This CICS COBOL program, CORPT00C, is part of the CardDemo application. Its primary function is to generate and print transaction reports by submitting batch jobs from an online interface using an extra partition Transient Data Queue (TDQ). The program allows users to select different report types (monthly, yearly, or custom date range) and submit the corresponding batch job for report generation.

<!-- general-rule-start -->
## General Business Rules:
1. The program provides three types of transaction reports:
   - Monthly Report: Generates a report for the current month up to the current date.
   - Yearly Report: Generates a report for the current year from January 1st to December 31st.
   - Custom Report: Allows users to specify a custom date range for the report.

2. User Interface:
   - The program uses a screen (CORPT0A) to interact with users.
   - Users can select the report type and enter custom date ranges if needed.
   - The program validates user inputs and displays error messages for invalid entries.

3. Date Validation:
   - For custom reports, the program validates the entered start and end dates.
   - It uses the CSUTLDTC utility to check if the dates are valid.

4. Job Submission:
   - After validation, the program generates a JCL (Job Control Language) script.
   - The JCL is written to a Transient Data Queue named 'JOBS' for submission.
   - The program uses a predefined JCL template and modifies it with the appropriate date parameters.

5. Confirmation:
   - Before submitting the job, the program asks for user confirmation.
   - Users must enter 'Y' or 'y' to confirm, 'N' or 'n' to cancel.

6. Error Handling:
   - The program checks for various error conditions and displays appropriate error messages.
   - It handles CICS response codes and reason codes for operations like writing to the TDQ.

7. Navigation:
   - Users can return to the previous screen (usually the sign-on screen) using the PF3 key.

8. Screen Management:
   - The program populates header information on the screen, including current date and time.
   - It manages cursor positioning and screen erasure based on the context.

9. Data Formatting:
   - Dates are formatted as YYYY-MM-DD for consistency in job submission.

10. Program Flow:
    - The program follows a typical CICS transaction flow with a main procedure, various sub-procedures for specific tasks, and a return to CICS at the end of processing.
<!-- general-rule-end -->

This program plays a crucial role in the CardDemo application by providing a user-friendly interface for generating transaction reports, which are likely used for analysis, auditing, and business intelligence purposes.
## Dependencies

This program relies on several external dependencies for its functionality, including copybooks for data structures, utility programs, and CICS resources. These dependencies are crucial for the program's operation and will need to be considered in any modernization efforts.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| COCOM01Y | Common communication area | Copybook | COCOM01Y | Contains shared data structures; may need to be replaced or adapted in a modernized system |
| CORPT00 | Screen map for transaction report | Copybook | CORPT00 | Defines the user interface; may need to be replaced with a modern UI technology |
| COTTL01Y | Title information | Copybook | COTTL01Y | Contains screen title information; may need to be updated or replaced |
| CSDAT01Y | Date handling routines | Copybook | CSDAT01Y | Date manipulation logic; may need to be replaced with modern date libraries |
| CSMSG01Y | Message handling | Copybook | CSMSG01Y | Message definitions; may need to be centralized in a message service |
| CVTRA05Y | Transaction record structure | Copybook | CVTRA05Y | Data structure for transactions; may need to be adapted to a modern data model |
| DFHAID | CICS AID constants | Copybook | DFHAID | CICS-specific; may need to be replaced in a non-CICS environment |
| DFHBMSCA | CICS BMS screen attributes | Copybook | DFHBMSCA | CICS-specific; may need to be replaced in a non-CICS environment |
| CSUTLDTC | Date validation utility | Program | N/A | External utility for date validation; may need to be replaced with a modern date library |
| JOBS | Transient Data Queue | CICS Resource | N/A | Used for job submission; may need to be replaced with a modern job scheduling system |
| CICS | Transaction processing environment | System | N/A | Core runtime environment; modernization may involve moving away from CICS |

These dependencies highlight the program's reliance on CICS-specific features and legacy data structures. In a modernization effort, special attention should be given to replacing CICS-specific functionalities, updating data structures, and potentially redesigning the user interface. The job submission process, currently using a TDQ, might be replaced with a more modern job scheduling or workflow system.
## Detailed Rules
These detailed rules provide a comprehensive breakdown of the main functionalities in the CORPT00C program. They cover the process of handling user input, validating data, preparing job submission parameters, and interacting with the CICS environment for screen display and job queue management. Each rule includes step-by-step algorithms that can guide the implementation in a modernized system, ensuring that all business logic and data handling processes are accurately translated.
## Data Structure

This section describes the key data structures used in the CORPT00C program. The data is primarily derived from the Working Storage Section and the copybooks referenced in the program.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| WS-PGMNAME | Program Name | Alphanumeric | X(08) | Name of the current program | Yes | 'CORPT00C' | Used for identification and logging | Yes | Internal |
| WS-TRANID | Transaction ID | Alphanumeric | X(04) | ID of the current transaction | Yes | 'CR00' | Used for CICS transaction management | Yes | Internal |
| WS-MESSAGE | Message | Alphanumeric | X(80) | Holds messages for display on screen | No | SPACES | Used for user communication | Yes | Display |
| WS-TRANSACT-FILE | Transaction File | Alphanumeric | X(08) | Name of the transaction file | Yes | 'TRANSACT' | File containing transaction data | Yes | Configuration |
| WS-ERR-FLG | Error Flag | Alphanumeric | X(01) | Indicates presence of an error | Yes | 'N' | 'Y' for error, 'N' for no error | Yes | Control |
| WS-TRANSACT-EOF | Transaction EOF | Alphanumeric | X(01) | End of file indicator for transaction file | Yes | 'N' | 'Y' for EOF, 'N' for not EOF | Yes | Control |
| WS-SEND-ERASE-FLG | Send Erase Flag | Alphanumeric | X(01) | Controls screen erasure on send | Yes | 'Y' | 'Y' to erase, 'N' to not erase | Yes | Control |
| WS-END-LOOP | End Loop Flag | Alphanumeric | X(01) | Controls loop termination | Yes | 'N' | 'Y' to end loop, 'N' to continue | Yes | Control |
| WS-RESP-CD | Response Code | Numeric | S9(09) COMP | Holds CICS response code | No | ZEROS | Used for error handling | Yes | System |
| WS-REAS-CD | Reason Code | Numeric | S9(09) COMP | Holds CICS reason code | No | ZEROS | Used for error handling | Yes | System |
| WS-REC-COUNT | Record Count | Numeric | S9(04) COMP | Counts processed records | No | ZEROS | Used for processing control | Yes | Internal |
| WS-IDX | Index | Numeric | S9(04) COMP | General purpose index | No | ZEROS | Used in loops | Yes | Internal |
| WS-REPORT-NAME | Report Name | Alphanumeric | X(10) | Holds the name of the selected report | No | SPACES | Used for report identification | Yes | Internal |
| WS-START-DATE | Start Date | Group Item | - | Holds the start date for report | Yes | - | Format: YYYY-MM-DD | Yes | Input |
| WS-END-DATE | End Date | Group Item | - | Holds the end date for report | Yes | - | Format: YYYY-MM-DD | Yes | Input |
| WS-DATE-FORMAT | Date Format | Alphanumeric | X(10) | Specifies the date format | Yes | 'YYYY-MM-DD' | Used for date validation | Yes | Configuration |
| WS-TRAN-AMT | Transaction Amount | Numeric Edited | +99999999.99 | Holds transaction amount | No | - | Used for display | Yes | Display |
| WS-TRAN-DATE | Transaction Date | Alphanumeric | X(08) | Holds transaction date | No | '00/00/00' | Format: MM/DD/YY | Yes | Display |
| JCL-RECORD | JCL Record | Alphanumeric | X(80) | Holds a single line of JCL | No | SPACES | Used for job submission | Yes | System |
| JOB-DATA | Job Data | Group Item | - | Contains the entire JCL template | Yes | - | Used for job submission | Yes | System |
| CSUTLDTC-PARM | CSUTLDTC Parameters | Group Item | - | Parameters for date validation utility | No | - | Used for date validation | Yes | System |
| CARDDEMO-COMMAREA | Card Demo Commarea | Group Item | - | Communication area for program | Yes | - | Defined in COCOM01Y | Yes | Communication |
| CORPT0AI | Report Screen Input | Group Item | - | Input fields for report screen | Yes | - | Defined in CORPT00 | Yes | Input |
| CORPT0AO | Report Screen Output | Group Item | - | Output fields for report screen | Yes | - | Defined in CORPT00 | Yes | Output |
| TRAN-RECORD | Transaction Record | Group Item | - | Structure of a transaction record | Yes | - | Defined in CVTRA05Y | Yes | Data |

This data structure provides a comprehensive overview of the key fields and data items used in the CORPT00C program. It includes working storage variables, screen input/output fields, and structures from referenced copybooks. All these elements are relevant for modernization as they represent the core data handling and business logic of the application.



<!--rule-start-->
## Reviewed Rule 1
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - Name: Process-Enter-Key

- Detailed Description of the Rule:
  This rule handles the processing when the user presses the Enter key on the transaction report screen. It determines which type of report the user has selected and prepares the necessary data for job submission.

- What it proposes to do:
  It validates user inputs, sets up date ranges for the selected report type, and initiates the job submission process.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Check which report type is selected:
   a. If MONTHLYI is not spaces or low-values:
      - Set WS-REPORT-NAME to 'Monthly'
      - Get current date using FUNCTION CURRENT-DATE
      - Set start date to first day of current month:
        - WS-START-DATE-YYYY = current year
        - WS-START-DATE-MM = current month
        - WS-START-DATE-DD = '01'
      - Set end date to last day of current month:
        - Add 1 to current month
        - If resulting month > 12, add 1 to year and set month to 1
        - Subtract 1 day from the first day of next month to get last day of current month
      - Move start and end dates to PARM-START-DATE-1, PARM-START-DATE-2, PARM-END-DATE-1, and PARM-END-DATE-2
      - Perform SUBMIT-JOB-TO-INTRDR

   b. If YEARLYI is not spaces or low-values:
      - Set WS-REPORT-NAME to 'Yearly'
      - Get current date using FUNCTION CURRENT-DATE
      - Set start date to January 1st of current year:
        - WS-START-DATE-YYYY = current year
        - WS-START-DATE-MM = '01'
        - WS-START-DATE-DD = '01'
      - Set end date to December 31st of current year:
        - WS-END-DATE-YYYY = current year
        - WS-END-DATE-MM = '12'
        - WS-END-DATE-DD = '31'
      - Move start and end dates to PARM-START-DATE-1, PARM-START-DATE-2, PARM-END-DATE-1, and PARM-END-DATE-2
      - Perform SUBMIT-JOB-TO-INTRDR

   c. If CUSTOMI is not spaces or low-values:
      - Validate all date fields (SDTMMI, SDTDDI, SDTYYYYI, EDTMMI, EDTDDI, EDTYYYYI) are not empty
      - Convert date fields from character to numeric using FUNCTION NUMVAL-C
      - Validate month values are between 1 and 12
      - Validate day values are between 1 and 31
      - Validate year values are numeric
      - Construct start and end dates in YYYY-MM-DD format
      - Validate dates using CSUTLDTC utility:
        - Call CSUTLDTC with start date
        - Call CSUTLDTC with end date
        - Check CSUTLDTC-RESULT-SEV-CD for each call
      - If dates are valid:
        - Move start and end dates to PARM-START-DATE-1, PARM-START-DATE-2, PARM-END-DATE-1, and PARM-END-DATE-2
        - Set WS-REPORT-NAME to 'Custom'
        - If no errors, perform SUBMIT-JOB-TO-INTRDR
      - For each invalid field, set appropriate error message and cursor position

   d. If no report type is selected:
      - Set error message to 'Select a report type to print report...'
      - Set cursor to MONTHLYL of CORPT0AI

2. If no errors occurred:
   - Perform INITIALIZE-ALL-FIELDS
   - Set ERRMSGC of CORPT0AO to DFHGREEN
   - Construct success message with WS-REPORT-NAME
   - Set cursor position to MONTHLYL of CORPT0AI
   - Perform SEND-TRNRPT-SCREEN

3. If any error occurs during the process:
   - Set appropriate error message in WS-MESSAGE
   - Set WS-ERR-FLG to 'Y'
   - Set cursor to the field with error
   - Perform SEND-TRNRPT-SCREEN

4. SUBMIT-JOB-TO-INTRDR:
   - If CONFIRMI is empty:
     - Set error message asking for confirmation
     - Set cursor to CONFIRML of CORPT0AI
   - If CONFIRMI is 'Y' or 'y':
     - Continue with job submission
   - If CONFIRMI is 'N' or 'n':
     - Initialize all fields and return to screen
   - If CONFIRMI is any other value:
     - Set error message for invalid confirmation
     - Set cursor to CONFIRML of CORPT0AI
   - If no errors:
     - Write job submission JCL to TDQ named 'JOBS'
     - If TDQ write fails, set error message

5. After processing:
   - If no errors, return to CICS with updated CARDDEMO-COMMAREA
   - If errors, send updated screen with error messages and cursor position
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
- ## Rule / Function / Method - Name: Submit-Job-To-Intrdr

- Detailed Description of the Rule:
  This rule handles the job submission process after the user has confirmed the report generation.

- What it proposes to do:
  It validates the user's confirmation, prepares the JCL (Job Control Language) for the batch job, and submits it to the internal reader via a Transient Data Queue.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Check if CONFIRMI of CORPT0AI is spaces or low-values:
   - If yes, set error message asking for confirmation
   - Set WS-ERR-FLG to 'Y'
   - Set cursor to CONFIRML of CORPT0AI
   - Perform SEND-TRNRPT-SCREEN

2. If CONFIRMI is not empty:
   - Evaluate CONFIRMI:
     a. If 'Y' or 'y': Continue to job submission
     b. If 'N' or 'n': 
        - Perform INITIALIZE-ALL-FIELDS
        - Set WS-ERR-FLG to 'Y'
        - Perform SEND-TRNRPT-SCREEN
     c. If any other value:
        - Set error message for invalid confirmation
        - Set WS-ERR-FLG to 'Y'
        - Set cursor to CONFIRML of CORPT0AI
        - Perform SEND-TRNRPT-SCREEN

3. If confirmation is 'Y' or 'y':
   - Initialize END-LOOP-NO
   - Loop through JOB-LINES array (max 1000 iterations):
     a. Move current JOB-LINES(WS-IDX) to JCL-RECORD
     b. If JCL-RECORD is '/*EOF' or spaces or low-values:
        - Set END-LOOP-YES
        - Exit loop
     c. Perform WIRTE-JOBSUB-TDQ for current JCL-RECORD
     d. If ERR-FLG-ON is set during WIRTE-JOBSUB-TDQ:
        - Exit loop

4. WIRTE-JOBSUB-TDQ:
   - Write JCL-RECORD to Transient Data Queue named 'JOBS'
   - If write operation is not successful:
     a. Set WS-ERR-FLG to 'Y'
     b. Set error message for unable to write to TDQ
     c. Set cursor to MONTHLYL of CORPT0AI
     d. Perform SEND-TRNRPT-SCREEN

Note: The actual JCL content is predefined in the JOB-DATA structure and includes parameters for the report date range. The JCL includes job information, JOBLIB specifications, and symbolic parameters for transaction date range.

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
Here's the revised business rule incorporating all the details and logic from the COBOL program:

```markdown
- ## Rule / Function / Method - Name: Write-Jobsub-TDQ

- Detailed Description of the Rule:
  This rule writes JCL records to the Transient Data Queue for job submission.

- What it proposes to do:
  It uses CICS WRITEQ TD command to write multiple JCL records to the 'JOBS' queue.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Check if the user has confirmed the report printing:
   - If CONFIRMI field is empty or low-values:
     - Display an error message asking for confirmation
     - Set cursor to CONFIRML field
     - Perform SEND-TRNRPT-SCREEN
   - If CONFIRMI is 'Y' or 'y', continue processing
   - If CONFIRMI is 'N' or 'n', initialize all fields and perform SEND-TRNRPT-SCREEN
   - For any other value in CONFIRMI, display an error message and set cursor to CONFIRML field

2. If confirmation is valid, proceed with job submission:
   - Initialize END-LOOP-NO flag
   - Loop through JOB-LINES array (up to 1000 entries) until END-LOOP-YES or ERR-FLG-ON:
     a. Move current JOB-LINES entry to JCL-RECORD
     b. If JCL-RECORD is '/*EOF' or spaces or low-values, set END-LOOP-YES
     c. Perform WRITE-JOBSUB-TDQ for each JCL-RECORD

3. For each JCL record, execute CICS WRITEQ TD command:
   - Set QUEUE to 'JOBS'
   - Set FROM to JCL-RECORD
   - Set LENGTH to the length of JCL-RECORD
   - Capture RESP in WS-RESP-CD
   - Capture RESP2 in WS-REAS-CD

4. Evaluate WS-RESP-CD:
   a. If DFHRESP(NORMAL):
      - Continue processing next JCL record
   b. For any other response:
      - Display RESP and REAS codes
      - Set WS-ERR-FLG to 'Y'
      - Set WS-MESSAGE to 'Unable to Write TDQ (JOBS)...'
      - Set cursor to MONTHLYL of CORPT0AI
      - Perform SEND-TRNRPT-SCREEN

5. After successful submission:
   - Initialize all input fields
   - Set ERRMSGC of CORPT0AO to DFHGREEN
   - Set WS-MESSAGE to "[Report Name] report submitted for printing ..."
   - Set cursor to MONTHLYL of CORPT0AI
   - Perform SEND-TRNRPT-SCREEN
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
Here's the revised business rule incorporating all the details from the COBOL program:

```markdown
- ## Rule / Function / Method - Name: Send-Trnrpt-Screen

- Detailed Description of the Rule:
  This rule handles the display of the transaction report screen to the user and processes user input for generating transaction reports.

- What it proposes to do:
  It populates the screen with necessary information, sends it to the terminal, and handles user input for report generation.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Perform POPULATE-HEADER-INFO
   - Set TITLE01O and TITLE02O from CCDA-TITLE01 and CCDA-TITLE02
   - Set TRNNAMEO to 'CR00'
   - Set PGMNAMEO to 'CORPT00C'
   - Set CURDATEO to current date in MM/DD/YY format
   - Set CURTIMEO to current time in HH:MM:SS format

2. Move WS-MESSAGE to ERRMSGO of CORPT0AO

3. If SEND-ERASE-YES is true:
   - Execute CICS SEND command:
     - Set MAP to 'CORPT0A'
     - Set MAPSET to 'CORPT00'
     - Set FROM to CORPT0AO
     - Include ERASE option
     - Include CURSOR option
   Else:
   - Execute CICS SEND command:
     - Set MAP to 'CORPT0A'
     - Set MAPSET to 'CORPT00'
     - Set FROM to CORPT0AO
     - Include CURSOR option
     - Do not include ERASE option

4. Process user input:
   - If MONTHLY option selected:
     - Set report name to 'Monthly'
     - Set start date to first day of current month
     - Set end date to last day of current month
   - If YEARLY option selected:
     - Set report name to 'Yearly'
     - Set start date to January 1st of current year
     - Set end date to December 31st of current year
   - If CUSTOM option selected:
     - Validate all date fields (start and end dates)
     - Set report name to 'Custom'
     - Use user-provided start and end dates

5. Validate user input:
   - Check if all required fields are filled
   - Validate date formats and ranges
   - Display appropriate error messages for invalid inputs

6. If input is valid and confirmed:
   - Generate JCL job for report
   - Submit job to internal reader (TDQ named 'JOBS')
   - Display confirmation message

7. If any errors occur:
   - Set appropriate error message
   - Highlight the field in error

8. Go to RETURN-TO-CICS
   - Return to CICS with TRANSID 'CR00' and the COMMAREA

9. Initialize all fields when necessary:
   - Clear all input fields
   - Reset error messages
   - Set cursor to MONTHLY field

10. Handle PF3 key to return to previous screen (program 'COMEN01C')

11. For any other key pressed, display 'Invalid key' error message
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Name: Populate-Header-Info

- Detailed Description of the Rule:
  This rule populates the header information for the transaction report screen.

- What it proposes to do:
  It sets up the current date and time, and populates various fields in the screen header.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Get current date and time using FUNCTION CURRENT-DATE and store in WS-CURDATE-DATA

2. Move CCDA-TITLE01 to TITLE01O of CORPT0AO
3. Move CCDA-TITLE02 to TITLE02O of CORPT0AO
4. Move WS-TRANID to TRNNAMEO of CORPT0AO
5. Move WS-PGMNAME to PGMNAMEO of CORPT0AO

6. Format current date:
   - Move WS-CURDATE-MONTH to WS-CURDATE-MM
   - Move WS-CURDATE-DAY to WS-CURDATE-DD
   - Move last two digits of WS-CURDATE-YEAR to WS-CURDATE-YY
   - Move formatted date (MM/DD/YY) to CURDATEO of CORPT0AO

7. Format current time:
   - Move WS-CURTIME-HOURS to WS-CURTIME-HH
   - Move WS-CURTIME-MINUTE to WS-CURTIME-MM
   - Move WS-CURTIME-SECOND to WS-CURTIME-SS
   - Move formatted time (HH:MM:SS) to CURTIMEO of CORPT0AO

8. Note: This rule is implemented in the POPULATE-HEADER-INFO paragraph of the COBOL program CORPT00C.CBL
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 6
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Name: Initialize-All-Fields

- Detailed Description of the Rule:
  This rule initializes all input fields on the transaction report screen and sets the cursor position.

- What it proposes to do:
  It resets all user input fields to their initial state, positions the cursor, and clears the message field.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Set cursor position:
   - Move -1 to MONTHLYL of CORPT0AI

2. Initialize input fields:
   - Initialize MONTHLYI of CORPT0AI
   - Initialize YEARLYI of CORPT0AI
   - Initialize CUSTOMI of CORPT0AI
   - Initialize SDTMMI of CORPT0AI
   - Initialize SDTDDI of CORPT0AI
   - Initialize SDTYYYYI of CORPT0AI
   - Initialize EDTMMI of CORPT0AI
   - Initialize EDTDDI of CORPT0AI
   - Initialize EDTYYYYI of CORPT0AI
   - Initialize CONFIRMI of CORPT0AI

3. Clear message:
   - Initialize WS-MESSAGE

4. Additional details:
   - This rule is implemented in the INITIALIZE-ALL-FIELDS paragraph of the COBOL program.
   - The initialization is performed using the INITIALIZE verb for each field.
   - The cursor is positioned at the MONTHLY field by moving -1 to its length field (MONTHLYL).
   - The WS-MESSAGE field, which is used for displaying messages to the user, is also initialized.
```

<!--rule-end-->