# COUSR03C.cbl: User Deletion Program

## Overview
COUSR03C.cbl is a CICS COBOL program that is part of the CardDemo application. Its primary function is to delete a user from the USRSEC file. This program allows administrators to remove user accounts from the system, ensuring proper management of user access and maintaining data integrity.

The program interacts with a user interface to receive input, processes the deletion request, and updates the USRSEC file accordingly. It also handles various error conditions and provides appropriate feedback to the user.

<!-- general-rule-start -->
## General Business Rules:

1. User Identification:
   - The program requires a valid user ID to be entered.
   - If the user ID field is empty, an error message is displayed.

2. User Verification:
   - Before deletion, the program verifies if the user exists in the USRSEC file.
   - If the user is not found, an error message is displayed.

3. Deletion Process:
   - The actual deletion is initiated when the user presses the PF5 key.
   - The program reads the user record, deletes it from the USRSEC file, and confirms the deletion.

4. User Interface:
   - The program displays a screen (COUSR3A) for user input and information display.
   - It shows the user's first name, last name, and user type before deletion.

5. Navigation:
   - PF3 key returns to the previous screen (usually COADM01C).
   - PF4 key clears the current screen.
   - PF12 key returns to the admin menu (COADM01C).

6. Error Handling:
   - The program handles various error conditions, such as invalid user ID, user not found, and database access issues.
   - Appropriate error messages are displayed to guide the user.

7. Security:
   - The program is part of the admin menu, implying that only authorized personnel should have access to this functionality.

8. Audit Trail:
   - While not explicitly shown in the code, it's advisable to log user deletion activities for audit purposes.

9. Confirmation:
   - The program requires the user to confirm the deletion by pressing PF5, preventing accidental deletions.

10. Data Integrity:
    - The program ensures that only existing users can be deleted, maintaining data integrity in the USRSEC file.

This program plays a crucial role in user management within the CardDemo application, allowing administrators to remove user accounts when necessary, such as when employees leave the organization or accounts are no longer needed.
<!-- general-rule-end -->
## Dependencies

This program relies on several dependencies, including copybooks for data structures, system interfaces, and file access. These dependencies are crucial for the program's functionality and will need to be considered in any modernization efforts.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| COCOM01Y | Common communication area | Copybook | COCOM01Y | Contains shared data structures; may need to be replaced with a more modern data sharing mechanism |
| COUSR03 | User interface map for deletion screen | Copybook | COUSR03 | Defines the screen layout; may need to be replaced with a web-based interface |
| COTTL01Y | Common title information | Copybook | COTTL01Y | Contains title information; may need to be updated for new UI |
| CSDAT01Y | Date handling routines | Copybook | CSDAT01Y | Date handling; may need updates for modern date formats |
| CSMSG01Y | Common messages | Copybook | CSMSG01Y | Message definitions; may need to be centralized in a message service |
| CSUSR01Y | User data structures | Copybook | CSUSR01Y | User data definitions; may need to be updated for new user management system |
| DFHAID | CICS AID constants | Copybook | DFHAID | CICS-specific; may need replacement in non-CICS environment |
| DFHBMSCA | CICS BMS screen attributes | Copybook | DFHBMSCA | CICS-specific; may need replacement for web-based UI |
| USRSEC | User security file | File | USRSEC | User data storage; may need to be replaced with a modern database |
| CICS | Transaction processing system | System | N/A | Core system; modernization may involve moving away from CICS |

These dependencies highlight the program's reliance on CICS and traditional mainframe structures. In a modernization effort, many of these would likely be replaced with more contemporary equivalents, such as web services, modern databases, and updated UI frameworks.
## Detailed Rules
## Data Structure

This section details the key data structures used in the COUSR03C program, focusing on the main working storage variables and the user interface fields.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| WS-PGMNAME | Program Name | Alphanumeric | 8 | Current program name | Yes | 'COUSR03C' | Used for logging and navigation | Yes | Control |
| WS-TRANID | Transaction ID | Alphanumeric | 4 | Current transaction ID | Yes | 'CU03' | Used for CICS transaction management | Yes | Control |
| WS-MESSAGE | Message | Alphanumeric | 80 | Holds messages for user display | No | SPACES | Used for user feedback | Yes | Display |
| WS-USRSEC-FILE | User Security File | Alphanumeric | 8 | Name of the user security file | Yes | 'USRSEC' | File containing user data | Yes | File |
| WS-ERR-FLG | Error Flag | Alphanumeric | 1 | Indicates presence of an error | Yes | 'N' | 'Y' for error, 'N' for no error | Yes | Control |
| WS-RESP-CD | Response Code | Numeric | 9 | CICS response code | No | ZEROS | Used for error handling | Yes | Control |
| WS-REAS-CD | Reason Code | Numeric | 9 | CICS reason code | No | ZEROS | Used for detailed error information | Yes | Control |
| WS-USR-MODIFIED | User Modified Flag | Alphanumeric | 1 | Indicates if user data was modified | Yes | 'N' | 'Y' if modified, 'N' if not | Yes | Control |
| USRIDINI | User ID Input | Alphanumeric | 8 | User ID for deletion | Yes | N/A | Primary key for user lookup | Yes | Input |
| FNAMEI | First Name Input | Alphanumeric | 20 | User's first name | No | N/A | Display only in this context | Yes | Display |
| LNAMEI | Last Name Input | Alphanumeric | 20 | User's last name | No | N/A | Display only in this context | Yes | Display |
| USRTYPEI | User Type Input | Alphanumeric | 1 | Type of user account | No | N/A | Display only in this context | Yes | Display |
| ERRMSGO | Error Message Output | Alphanumeric | 78 | Displays error or information messages | No | SPACES | Used for user feedback | Yes | Display |
| TITLE01O | Title 1 Output | Alphanumeric | 40 | First line of screen title | Yes | N/A | From CCDA-TITLE01 | Yes | Display |
| TITLE02O | Title 2 Output | Alphanumeric | 40 | Second line of screen title | Yes | N/A | From CCDA-TITLE02 | Yes | Display |
| TRNNAMEO | Transaction Name Output | Alphanumeric | 4 | Displays current transaction name | Yes | N/A | From WS-TRANID | Yes | Display |
| PGMNAMEO | Program Name Output | Alphanumeric | 8 | Displays current program name | Yes | N/A | From WS-PGMNAME | Yes | Display |
| CURDATEO | Current Date Output | Alphanumeric | 8 | Displays current date | Yes | N/A | Format: MM/DD/YY | Yes | Display |
| CURTIMEO | Current Time Output | Alphanumeric | 8 | Displays current time | Yes | N/A | Format: HH:MM:SS | Yes | Display |



<!--rule-start-->
## Reviewed Rule 1
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - MAIN-PARA:
   - Detailed Description of the Rule:
     This is the main entry point of the program. It initializes flags, handles the program flow based on whether it's the first entry or a re-entry, and processes user actions.
   - What it proposes to do:
     Control the overall flow of the user deletion process.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Initialize error flag (ERR-FLG-OFF) and user modification flag (USR-MODIFIED-NO)
   2. Clear message fields (WS-MESSAGE and ERRMSGO of COUSR3AO)
   3. Check if EIBCALEN (CICS Communication Area Length) is 0
      - If yes, set CDEMO-TO-PROGRAM to 'COSGN00C' and perform RETURN-TO-PREV-SCREEN
   4. If EIBCALEN is not 0:
      - Move DFHCOMMAREA to CARDDEMO-COMMAREA
      - If not CDEMO-PGM-REENTER:
        a. Set CDEMO-PGM-REENTER to TRUE
        b. Clear COUSR3AO by moving LOW-VALUES to it
        c. Set cursor position to USRIDINL of COUSR3AI
        d. If CDEMO-CU03-USR-SELECTED is not empty (not SPACES and not LOW-VALUES):
           - Move CDEMO-CU03-USR-SELECTED to USRIDINI of COUSR3AI
           - Perform PROCESS-ENTER-KEY
        e. Perform SEND-USRDEL-SCREEN
      - If CDEMO-PGM-REENTER:
        a. Perform RECEIVE-USRDEL-SCREEN
        b. Process user action based on EIBAID:
           - DFHENTER: Perform PROCESS-ENTER-KEY
           - DFHPF3: 
             * If CDEMO-FROM-PROGRAM is SPACES or LOW-VALUES, set CDEMO-TO-PROGRAM to 'COADM01C'
             * Otherwise, set CDEMO-TO-PROGRAM to CDEMO-FROM-PROGRAM
             * Perform RETURN-TO-PREV-SCREEN
           - DFHPF4: Perform CLEAR-CURRENT-SCREEN
           - DFHPF5: Perform DELETE-USER-INFO
           - DFHPF12: Set CDEMO-TO-PROGRAM to 'COADM01C' and perform RETURN-TO-PREV-SCREEN
           - Other keys: Set WS-ERR-FLG to 'Y', move CCDA-MSG-INVALID-KEY to WS-MESSAGE, and perform SEND-USRDEL-SCREEN
   5. Return to CICS with TRANSID (WS-TRANID) and COMMAREA (CARDDEMO-COMMAREA)

   ## Additional Details:
   - The program uses various copybooks: COCOM01Y, COUSR03, COTTL01Y, CSDAT01Y, CSMSG01Y, CSUSR01Y, DFHAID, DFHBMSCA
   - The program interacts with a user security file named 'USRSEC'
   - PROCESS-ENTER-KEY:
     * Validates that USRIDINI is not empty
     * Reads user data from USRSEC file and populates screen fields (FNAMEI, LNAMEI, USRTYPEI)
   - DELETE-USER-INFO:
     * Validates that USRIDINI is not empty
     * Reads user data from USRSEC file
     * Deletes the user record from USRSEC file
   - The program uses various utility paragraphs like POPULATE-HEADER-INFO, READ-USER-SEC-FILE, DELETE-USER-SEC-FILE, CLEAR-CURRENT-SCREEN, and INITIALIZE-ALL-FIELDS
   - Error messages and success messages are displayed in WS-MESSAGE and sent to the screen
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - PROCESS-ENTER-KEY:
   - Detailed Description of the Rule:
     This function processes the user input when the Enter key is pressed. It validates the user ID and retrieves user information if valid.
   - What it proposes to do:
     Validate user input and display user information for the entered user ID.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Check if USRIDINI is empty or contains low-values
      - If yes, set error flag to 'Y', set WS-MESSAGE to 'User ID can NOT be empty...', set cursor to USRIDINI
      - If no, set cursor to USRIDINI and continue
   2. If no error:
      - Clear FNAMEI, LNAMEI, and USRTYPEI fields
      - Move USRIDINI to SEC-USR-ID
      - Perform READ-USER-SEC-FILE:
        - Read USRSEC file with SEC-USR-ID as key
        - If successful:
          - Set WS-MESSAGE to 'Press PF5 key to delete this user ...'
          - Set ERRMSGC of COUSR3AO to DFHNEUTR
        - If record not found:
          - Set error flag to 'Y'
          - Set WS-MESSAGE to 'User ID NOT found...'
          - Set cursor to USRIDINI
        - For other errors:
          - Set error flag to 'Y'
          - Set WS-MESSAGE to 'Unable to lookup User...'
          - Set cursor to FNAMEL
   3. If no error after reading user file:
      - Move SEC-USR-FNAME to FNAMEI
      - Move SEC-USR-LNAME to LNAMEI
      - Move SEC-USR-TYPE to USRTYPEI
   4. Perform SEND-USRDEL-SCREEN:
      - Populate header information (including current date and time)
      - Move WS-MESSAGE to ERRMSGO of COUSR3AO
      - Send map 'COUSR3A' from COUSR3AO
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
```markdown
- ## Rule / Function / Method - DELETE-USER-INFO:
   - Detailed Description of the Rule:
     This function handles the actual deletion of a user when the PF5 key is pressed.
   - What it proposes to do:
     Delete the specified user from the USRSEC file.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Check if USRIDINI is empty or contains low-values
      - If yes, set error flag to 'Y', set message to 'User ID can NOT be empty...', set cursor to USRIDINI
      - If no, set cursor to USRIDINI and continue
   2. If no error:
      - Move USRIDINI to SEC-USR-ID
      - Perform READ-USER-SEC-FILE
        - If user found, set message to 'Press PF5 key to delete this user ...' and display it in neutral color
        - If user not found, set error flag to 'Y', set message to 'User ID NOT found...', set cursor to USRIDINI
        - If other error, set error flag to 'Y', set message to 'Unable to lookup User...', set cursor to FNAMEL
      - Perform DELETE-USER-SEC-FILE
        - If successful, initialize all fields, set message color to green, and construct message: 'User [SEC-USR-ID] has been deleted ...'
        - If user not found, set error flag to 'Y', set message to 'User ID NOT found...', set cursor to USRIDINI
        - If other error, set error flag to 'Y', set message to 'Unable to Update User...', set cursor to FNAMEL
   3. Perform SEND-USRDEL-SCREEN to display the results
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - RETURN-TO-PREV-SCREEN:
   - Detailed Description of the Rule:
     This function handles the return to the previous screen or the sign-on screen.
   - What it proposes to do:
     Navigate back to the appropriate screen based on the program flow.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. If CDEMO-TO-PROGRAM is LOW-VALUES or SPACES, set it to 'COSGN00C'
   2. Move WS-TRANID to CDEMO-FROM-TRANID
   3. Move WS-PGMNAME to CDEMO-FROM-PROGRAM
   4. Set CDEMO-PGM-CONTEXT to ZEROS
   5. Execute CICS XCTL to transfer control to CDEMO-TO-PROGRAM with CARDDEMO-COMMAREA as the communication area
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
```markdown
- ## Rule / Function / Method - SEND-USRDEL-SCREEN:
   - Detailed Description of the Rule:
     This function sends the user deletion screen to the terminal, populating it with header information and any error messages.
   - What it proposes to do:
     Display the user interface for the deletion process, including current date/time, transaction details, and any relevant messages.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Perform POPULATE-HEADER-INFO
      - Set TITLE01O and TITLE02O from CCDA-TITLE01 and CCDA-TITLE02
      - Set TRNNAMEO to WS-TRANID (CU03)
      - Set PGMNAMEO to WS-PGMNAME (COUSR03C)
      - Set CURDATEO to current date in MM/DD/YY format
      - Set CURTIMEO to current time in HH:MM:SS format
   2. Move WS-MESSAGE to ERRMSGO of COUSR3AO
   3. Execute CICS SEND command:
      - MAP: 'COUSR3A'
      - MAPSET: 'COUSR03'
      - FROM: COUSR3AO
      - ERASE
      - CURSOR

   ## Additional Details:
   - The function is part of a larger process for user deletion in the COUSR03C program
   - It is called after various operations like processing user input, reading user data, or attempting to delete a user
   - The content of WS-MESSAGE varies based on the operation performed before calling this function, and can include error messages or success notifications
   - The color of the error message (ERRMSGC of COUSR3AO) may be set to DFHGREEN for success messages or DFHNEUTR for informational messages
   - The cursor position is not explicitly set in this function, but is influenced by operations performed before calling SEND-USRDEL-SCREEN
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 6
```markdown
- ## Rule / Function / Method - RECEIVE-USRDEL-SCREEN:
   - Detailed Description of the Rule:
     This function receives user input from the deletion screen and handles various user actions.
   - What it proposes to do:
     Capture user input for processing and execute appropriate actions based on the function key pressed.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Execute CICS RECEIVE command:
      - MAP: 'COUSR3A'
      - MAPSET: 'COUSR03'
      - INTO: COUSR3AI
      - RESP: WS-RESP-CD
      - RESP2: WS-REAS-CD
   2. Evaluate the EIBAID (Attention Identifier) to determine the user action:
      - DFHENTER (Enter key):
        - Execute PROCESS-ENTER-KEY
      - DFHPF3 (PF3 key):
        - If CDEMO-FROM-PROGRAM is spaces or low-values, set CDEMO-TO-PROGRAM to 'COADM01C'
        - Otherwise, set CDEMO-TO-PROGRAM to CDEMO-FROM-PROGRAM
        - Execute RETURN-TO-PREV-SCREEN
      - DFHPF4 (PF4 key):
        - Execute CLEAR-CURRENT-SCREEN
      - DFHPF5 (PF5 key):
        - Execute DELETE-USER-INFO
      - DFHPF12 (PF12 key):
        - Set CDEMO-TO-PROGRAM to 'COADM01C'
        - Execute RETURN-TO-PREV-SCREEN
      - Any other key:
        - Set WS-ERR-FLG to 'Y'
        - Set WS-MESSAGE to CCDA-MSG-INVALID-KEY
        - Execute SEND-USRDEL-SCREEN
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 7
```markdown
- ## Rule / Function / Method - POPULATE-HEADER-INFO:
   - Detailed Description of the Rule:
     This function populates the header information for the user interface.
   - What it proposes to do:
     Set up the common header elements of the screen.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Get current date and time using FUNCTION CURRENT-DATE
   2. Move CCDA-TITLE01 to TITLE01O of COUSR3AO
   3. Move CCDA-TITLE02 to TITLE02O of COUSR3AO
   4. Move WS-TRANID to TRNNAMEO of COUSR3AO
   5. Move WS-PGMNAME to PGMNAMEO of COUSR3AO
   6. Format current date (MM/DD/YY) and move to CURDATEO of COUSR3AO:
      - Extract month from WS-CURDATE-MONTH
      - Extract day from WS-CURDATE-DAY
      - Extract last two digits of year from WS-CURDATE-YEAR
      - Combine as MM/DD/YY
   7. Format current time (HH:MM:SS) and move to CURTIMEO of COUSR3AO:
      - Extract hours from WS-CURTIME-HOURS
      - Extract minutes from WS-CURTIME-MINUTE
      - Extract seconds from WS-CURTIME-SECOND
      - Combine as HH:MM:SS
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 8
- ## Rule / Function / Method - READ-USER-SEC-FILE:
   - Detailed Description of the Rule:
     This function reads the user record from the USRSEC file.
   - What it proposes to do:
     Retrieve user information for display and verification before deletion.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Execute CICS READ command:
      - DATASET: WS-USRSEC-FILE
      - INTO: SEC-USER-DATA
      - LENGTH: length of SEC-USER-DATA
      - RIDFLD: SEC-USR-ID
      - KEYLENGTH: length of SEC-USR-ID
      - UPDATE
      - RESP: WS-RESP-CD
      - RESP2: WS-REAS-CD
   2. Evaluate WS-RESP-CD:
      - If NORMAL:
        a. Set message to "Press PF5 key to delete this user ..."
        b. Set ERRMSGC to DFHNEUTR
        c. Perform SEND-USRDEL-SCREEN
      - If NOTFND:
        a. Set error flag (WS-ERR-FLG) to 'Y'
        b. Set message to "User ID NOT found..."
        c. Set cursor to USRIDINL
        d. Perform SEND-USRDEL-SCREEN
      - For other responses:
        a. Display error information (RESP and REAS codes)
        b. Set error flag (WS-ERR-FLG) to 'Y'
        c. Set message to "Unable to lookup User..."
        d. Set cursor to FNAMEL
        e. Perform SEND-USRDEL-SCREEN
   3. If no error occurred (WS-ERR-FLG is not 'Y'):
      a. Move SEC-USR-FNAME to FNAMEI of COUSR3AI
      b. Move SEC-USR-LNAME to LNAMEI of COUSR3AI
      c. Move SEC-USR-TYPE to USRTYPEI of COUSR3AI
      d. Perform SEND-USRDEL-SCREEN

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 9
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - DELETE-USER-SEC-FILE:
   - Detailed Description of the Rule:
     This function deletes the user record from the USRSEC file.
   - What it proposes to do:
     Remove the specified user from the security file.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Validate input:
      - If USRIDINI is empty or low-values:
        a. Set error flag to 'Y'
        b. Set message to "User ID can NOT be empty..."
        c. Set cursor to USRIDINL
        d. Perform SEND-USRDEL-SCREEN
      - Otherwise, continue processing
   
   2. Read user record:
      - Execute CICS READ command:
        - DATASET: WS-USRSEC-FILE (value 'USRSEC  ')
        - INTO: SEC-USER-DATA
        - RIDFLD: SEC-USR-ID (from USRIDINI)
        - UPDATE mode
      - Evaluate response (WS-RESP-CD):
        - If NORMAL:
          a. Set message to "Press PF5 key to delete this user ..."
          b. Set ERRMSGC to DFHNEUTR
          c. Perform SEND-USRDEL-SCREEN
        - If NOTFND:
          a. Set error flag to 'Y'
          b. Set message to "User ID NOT found..."
          c. Set cursor to USRIDINL
          d. Perform SEND-USRDEL-SCREEN
        - For other responses:
          a. Display RESP and REAS codes
          b. Set error flag to 'Y'
          c. Set message to "Unable to lookup User..."
          d. Set cursor to FNAMEL
          e. Perform SEND-USRDEL-SCREEN

   3. If no errors, execute CICS DELETE command:
      - DATASET: WS-USRSEC-FILE
      - RESP: WS-RESP-CD
      - RESP2: WS-REAS-CD
   
   4. Evaluate WS-RESP-CD:
      - If NORMAL:
        a. Perform INITIALIZE-ALL-FIELDS
        b. Clear WS-MESSAGE
        c. Set ERRMSGC to DFHGREEN
        d. Construct success message: "User [SEC-USR-ID] has been deleted ..."
        e. Perform SEND-USRDEL-SCREEN
      - If NOTFND:
        a. Set error flag to 'Y'
        b. Set message to "User ID NOT found..."
        c. Set cursor to USRIDINL
        d. Perform SEND-USRDEL-SCREEN
      - For other responses:
        a. Display RESP and REAS codes
        b. Set error flag to 'Y'
        c. Set message to "Unable to Update User..."
        d. Set cursor to FNAMEL
        e. Perform SEND-USRDEL-SCREEN

   5. INITIALIZE-ALL-FIELDS:
      - Set cursor to USRIDINL
      - Clear USRIDINI, FNAMEI, LNAMEI, USRTYPEI, and WS-MESSAGE

   6. SEND-USRDEL-SCREEN:
      - Populate header information (date, time, transaction ID, program name)
      - Move WS-MESSAGE to ERRMSGO
      - Send map 'COUSR3A' in mapset 'COUSR03' with erase option and cursor positioning
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 10
```markdown
- ## Rule / Function / Method - CLEAR-CURRENT-SCREEN:
    - Detailed Description of the Rule:
      This function clears all fields on the current screen and resets the user interface to its initial state.
    - What it proposes to do:
      Reset the user interface to its initial state by clearing all input fields and resetting the cursor position.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. Perform INITIALIZE-ALL-FIELDS
       - Set cursor position to USRIDINL field
       - Clear the following fields to spaces:
         - USRIDINI (User ID)
         - FNAMEI (First Name)
         - LNAMEI (Last Name)
         - USRTYPEI (User Type)
       - Clear WS-MESSAGE (error/info message field)
    2. Perform SEND-USRDEL-SCREEN
       - Populate header information (title, transaction name, program name, current date, and time)
       - Send the COUSR3A map to display the cleared screen
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 11
- ## Rule / Function / Method - INITIALIZE-ALL-FIELDS:
    - Detailed Description of the Rule:
      This function initializes all fields used in the program to their default values.
    - What it proposes to do:
      Reset all variables to their default values.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. Set cursor position to USRIDINL (-1)
    2. Move SPACES to:
       - USRIDINI of COUSR3AI
       - FNAMEI of COUSR3AI
       - LNAMEI of COUSR3AI
       - USRTYPEI of COUSR3AI
       - WS-MESSAGE

    ## Additional Details:
    - The function is called in two scenarios:
      1. After successfully deleting a user from the USRSEC file
      2. When clearing the current screen (PF4 key)
    - This function is crucial for maintaining a clean state of the screen and variables between operations.

<!--rule-end-->