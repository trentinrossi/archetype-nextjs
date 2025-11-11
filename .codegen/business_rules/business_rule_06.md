# COUSR02C.cbl: User Update Program

## Overview
This COBOL program, COUSR02C, is part of the CardDemo application and functions as a CICS transaction for updating user information in the USRSEC file. It allows authorized users to modify existing user records, including their first name, last name, password, and user type. The program handles user input, performs data validation, and updates the user security file accordingly.

<!-- general-rule-start -->
## General Business Rules:

1. User Authentication:
   - The program assumes that user authentication has already been performed before this program is called.
   - It uses a commarea to receive information from the calling program, including the selected user ID to be updated.

2. Screen Handling:
   - The program uses a map called COUSR2A for displaying and receiving user information.
   - It populates header information, including current date and time, on the screen.

3. User Record Retrieval:
   - When a user ID is entered or received from the previous screen, the program retrieves the corresponding user record from the USRSEC file.
   - If the user is not found, an error message is displayed.

4. Data Validation:
   - The program validates that all required fields (User ID, First Name, Last Name, Password, and User Type) are not empty.
   - If any required field is empty, an appropriate error message is displayed.

5. Update Process:
   - The program compares the entered data with the existing data in the USRSEC file.
   - If any field has been modified, the USR-MODIFIED-YES flag is set.
   - Updates are only performed if at least one field has been changed.

6. File Operations:
   - The program reads the USRSEC file with UPDATE option to lock the record for modification.
   - If changes are made, it rewrites the record to the USRSEC file.

7. Error Handling:
   - The program handles various error scenarios, such as record not found, unable to read or update the file, etc.
   - Appropriate error messages are displayed to the user.

8. Navigation:
   - PF3 key: Updates the user information (if modified) and returns to the previous screen.
   - PF4 key: Clears the current screen.
   - PF5 key: Updates the user information without leaving the screen.
   - PF12 key: Returns to the admin menu (COADM01C) without updating.
   - ENTER key: Retrieves and displays the user information for the entered User ID.

9. Security:
   - The program does not explicitly check for user permissions to perform updates.
   - It assumes that access control is handled by the calling program or the CICS environment.

10. Audit Trail:
    - The program does not implement any logging or audit trail for user updates.
    - Consideration should be given to adding this feature for security and compliance reasons.

This program plays a crucial role in maintaining user information within the CardDemo application, ensuring that authorized personnel can update user details as needed while maintaining data integrity and basic validation checks.
<!-- general-rule-end -->
## Dependencies

This program relies on several external dependencies, including CICS commands, copybooks, and a file system. These dependencies are crucial for the program's functionality and will need to be considered in any modernization efforts.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| CICS | Customer Information Control System, provides transaction management and user interface capabilities | Runtime Environment | N/A | High - May need to be replaced or emulated in a modernized system |
| COCOM01Y | Common communication area copybook | Copybook | COCOM01Y | Medium - Data structure may need to be adapted or replaced |
| COUSR02 | Screen map copybook for user update screen | Copybook | COUSR02 | High - UI elements may need to be redesigned for web or GUI interfaces |
| COTTL01Y | Copybook containing screen titles | Copybook | COTTL01Y | Low - Content can be easily migrated to new format |
| CSDAT01Y | Copybook likely containing date-related structures | Copybook | CSDAT01Y | Medium - Date handling may need to be updated |
| CSMSG01Y | Copybook containing common messages | Copybook | CSMSG01Y | Low - Messages can be easily migrated to new format |
| CSUSR01Y | Copybook likely containing user-related structures | Copybook | CSUSR01Y | High - Core data structure that may need redesign |
| DFHAID | CICS copybook for attention identifier constants | Copybook | DFHAID | Medium - May need to be replaced with modern UI event handling |
| DFHBMSCA | CICS copybook for BMS screen attribute constants | Copybook | DFHBMSCA | Medium - May need to be replaced with modern UI attributes |
| USRSEC | User security file | File | USRSEC | High - Data storage may need to be migrated to a database system |

These dependencies play crucial roles in the program's functionality, from defining data structures to handling user interfaces and data storage. In a modernization effort, special attention should be given to replacing CICS-specific features, updating the user interface, and potentially migrating the file-based data storage to a more modern database system. The copybooks will likely need to be translated into equivalent structures in the target modernization platform or language.
## Detailed Rules
## Data Structure

This section describes the key data structures used in the COUSR02C program. The information is derived from the copybooks and working storage section of the program.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| WS-PGMNAME | Program Name | PIC X(08) | 8 | Name of the current program | Yes | 'COUSR02C' | Used for logging and display | Medium | Control |
| WS-TRANID | Transaction ID | PIC X(04) | 4 | ID of the current transaction | Yes | 'CU02' | Used for CICS operations | High | Control |
| WS-MESSAGE | Message | PIC X(80) | 80 | Holds messages for display | No | SPACES | Used for user feedback | Medium | Display |
| WS-USRSEC-FILE | User Security File | PIC X(08) | 8 | Name of the user security file | Yes | 'USRSEC' | Critical for data operations | High | Control |
| WS-ERR-FLG | Error Flag | PIC X(01) | 1 | Indicates if an error occurred | Yes | 'N' | Used for flow control | Medium | Control |
| WS-RESP-CD | Response Code | PIC S9(09) COMP | 4 | Holds CICS response codes | No | ZEROS | Used for error handling | High | Control |
| WS-REAS-CD | Reason Code | PIC S9(09) COMP | 4 | Holds CICS reason codes | No | ZEROS | Used for error handling | High | Control |
| WS-USR-MODIFIED | User Modified Flag | PIC X(01) | 1 | Indicates if user data was modified | Yes | 'N' | Used for update logic | Medium | Control |
| USRIDINI | User ID Input | PIC X(8) | 8 | User ID entered by user | Yes | N/A | Key field for user lookup | High | Input |
| FNAMEI | First Name Input | PIC X(20) | 20 | User's first name | Yes | N/A | Part of user data | High | Input |
| LNAMEI | Last Name Input | PIC X(20) | 20 | User's last name | Yes | N/A | Part of user data | High | Input |
| PASSWDI | Password Input | PIC X(8) | 8 | User's password | Yes | N/A | Sensitive data | High | Input |
| USRTYPEI | User Type Input | PIC X(1) | 1 | Type of user | Yes | N/A | Controls user permissions | High | Input |
| SEC-USR-ID | Security User ID | PIC X(8) | 8 | User ID in security file | Yes | N/A | Key for file operations | High | Data |
| SEC-USR-FNAME | Security User First Name | PIC X(20) | 20 | User's first name in security file | Yes | N/A | Part of user data | High | Data |
| SEC-USR-LNAME | Security User Last Name | PIC X(20) | 20 | User's last name in security file | Yes | N/A | Part of user data | High | Data |
| SEC-USR-PWD | Security User Password | PIC X(8) | 8 | User's password in security file | Yes | N/A | Sensitive data | High | Data |
| SEC-USR-TYPE | Security User Type | PIC X(1) | 1 | User's type in security file | Yes | N/A | Controls user permissions | High | Data |

Note: The actual structure of SEC-USER-DATA is not fully visible in the provided code snippet, so some details about the security file fields are inferred. In a modernization effort, it would be crucial to obtain the complete definition of this structure.



<!--rule-start-->
## Reviewed Rule 1
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Name: PROCESS-ENTER-KEY

    - Detailed Description of the Rule:
      This rule handles the processing when the ENTER key is pressed on the user update screen.
    - What it proposes to do:
      Validate the entered User ID and retrieve the corresponding user information from the USRSEC file.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. Check if the User ID field (USRIDINI) is empty or contains only spaces:
       - If empty, set WS-ERR-FLG to 'Y'
       - Set WS-MESSAGE to 'User ID can NOT be empty...'
       - Set cursor position to USRIDINL field
       - Call SEND-USRUPD-SCREEN subroutine
       - Exit the subroutine
    2. If User ID is not empty:
       - Set cursor position to USRIDINL field
    3. Clear the following fields:
       - FNAMEI (First Name)
       - LNAMEI (Last Name)
       - PASSWDI (Password)
       - USRTYPEI (User Type)
    4. Move the entered User ID (USRIDINI) to SEC-USR-ID
    5. Call READ-USER-SEC-FILE subroutine
    6. If no error occurred during file read (ERR-FLG-OFF is true):
       - Move SEC-USR-FNAME to FNAMEI
       - Move SEC-USR-LNAME to LNAMEI
       - Move SEC-USR-PWD to PASSWDI
       - Move SEC-USR-TYPE to USRTYPEI
       - Set WS-MESSAGE to 'Press PF5 key to save your updates ...'
       - Set ERRMSGC of COUSR2AO to DFHNEUTR (neutral color)
       - Call SEND-USRUPD-SCREEN subroutine
    7. If an error occurs during file read:
       - If RESP is DFHRESP(NOTFND):
         - Set WS-ERR-FLG to 'Y'
         - Set WS-MESSAGE to 'User ID NOT found...'
         - Set cursor position to USRIDINL field
         - Call SEND-USRUPD-SCREEN subroutine
       - For any other error:
         - Set WS-ERR-FLG to 'Y'
         - Set WS-MESSAGE to 'Unable to lookup User...'
         - Set cursor position to FNAMEL field
         - Call SEND-USRUPD-SCREEN subroutine
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Name: UPDATE-USER-INFO

    - Detailed Description of the Rule:
      This rule handles the process of updating user information in the USRSEC file.
    - What it proposes to do:
      Validate all input fields, compare with existing data, and update the USRSEC file if changes are detected.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. Validate input fields:
       - If USRIDINI is empty or low-values, set error message "User ID can NOT be empty..." and return
       - If FNAMEI is empty or low-values, set error message "First Name can NOT be empty..." and return
       - If LNAMEI is empty or low-values, set error message "Last Name can NOT be empty..." and return
       - If PASSWDI is empty or low-values, set error message "Password can NOT be empty..." and return
       - If USRTYPEI is empty or low-values, set error message "User Type can NOT be empty..." and return
    2. If all fields are valid, set cursor to FNAMEL field
    3. Move USRIDINI to SEC-USR-ID
    4. Call READ-USER-SEC-FILE subroutine:
       - If user is found, display message "Press PF5 key to save your updates ..." in neutral color
       - If user is not found, set error message "User ID NOT found..." and return
       - If other error occurs, set error message "Unable to lookup User..." and return
    5. Compare entered data with existing data:
       - If FNAMEI â  SEC-USR-FNAME, update SEC-USR-FNAME and set USR-MODIFIED-YES
       - If LNAMEI â  SEC-USR-LNAME, update SEC-USR-LNAME and set USR-MODIFIED-YES
       - If PASSWDI â  SEC-USR-PWD, update SEC-USR-PWD and set USR-MODIFIED-YES
       - If USRTYPEI â  SEC-USR-TYPE, update SEC-USR-TYPE and set USR-MODIFIED-YES
    6. If USR-MODIFIED-YES is true:
       - Call UPDATE-USER-SEC-FILE subroutine:
         - If update is successful, set message "User [SEC-USR-ID] has been updated ..." in green color
         - If user is not found, set error message "User ID NOT found..." and return
         - If other error occurs, set error message "Unable to Update User..." and return
    7. If no modifications were made:
       - Set WS-MESSAGE to 'Please modify to update ...'
       - Set ERRMSGC to DFHRED (red color)
    8. Call SEND-USRUPD-SCREEN subroutine to display the updated information or error messages

    ## Additional Control Flow:
    - If EIBCALEN = 0, return to program 'COSGN00C'
    - If CDEMO-PGM-REENTER is false:
      - Initialize COUSR2AO with LOW-VALUES
      - Set cursor to USRIDINL
      - If CDEMO-CU02-USR-SELECTED is not empty or low-values:
        - Move CDEMO-CU02-USR-SELECTED to USRIDINI
        - Perform PROCESS-ENTER-KEY
    - Handle different function keys:
      - ENTER: Perform PROCESS-ENTER-KEY
      - PF3: Perform UPDATE-USER-INFO, then return to previous screen (COADM01C or CDEMO-FROM-PROGRAM)
      - PF4: Perform CLEAR-CURRENT-SCREEN
      - PF5: Perform UPDATE-USER-INFO
      - PF12: Return to COADM01C
      - Other keys: Display invalid key message

    ## Error Handling:
    - For any error condition, set WS-ERR-FLG to 'Y' and display appropriate error message
    - For database errors, check RESP and RESP2 codes for specific error conditions
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
- ## Rule / Function / Method - Name: READ-USER-SEC-FILE

    - Detailed Description of the Rule:
      This rule reads a user record from the USRSEC file.
    - What it proposes to do:
      Retrieve user information based on the provided User ID.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. Execute CICS READ command with the following parameters:
       - DATASET: WS-USRSEC-FILE
       - INTO: SEC-USER-DATA
       - LENGTH: LENGTH OF SEC-USER-DATA
       - RIDFLD: SEC-USR-ID
       - KEYLENGTH: LENGTH OF SEC-USR-ID
       - UPDATE (to lock the record for potential modification)
    2. Check the response code (WS-RESP-CD):
       - If NORMAL:
         - Set WS-MESSAGE to 'Press PF5 key to save your updates ...'
         - Set ERRMSGC to DFHNEUTR (neutral color)
         - Call SEND-USRUPD-SCREEN subroutine
       - If NOTFND:
         - Set WS-ERR-FLG to 'Y'
         - Set WS-MESSAGE to 'User ID NOT found...'
         - Set cursor to USRIDINL field
         - Call SEND-USRUPD-SCREEN subroutine
       - For any other response:
         - Display error codes (WS-RESP-CD and WS-REAS-CD) on the console
         - Set WS-ERR-FLG to 'Y'
         - Set WS-MESSAGE to 'Unable to lookup User...'
         - Set cursor to FNAMEL field
         - Call SEND-USRUPD-SCREEN subroutine

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
Here's the revised business rule with all necessary corrections and details incorporated:

```markdown
- ## Rule / Function / Method - Name: UPDATE-USER-SEC-FILE

    - Detailed Description of the Rule:
      This rule updates a user record in the USRSEC file after validating input fields and checking for modifications.
    - What it proposes to do:
      Validate user input, check for modifications, and write the modified user information back to the USRSEC file if changes are detected.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. Validate input fields:
       - Check if User ID is not empty
       - Check if First Name is not empty
       - Check if Last Name is not empty
       - Check if Password is not empty
       - Check if User Type is not empty
       - If any field is empty, set error flag, display appropriate error message, and return to the update screen
    2. Read the existing user record from USRSEC file using the input User ID
    3. Compare input fields with existing record:
       - Check if First Name has changed
       - Check if Last Name has changed
       - Check if Password has changed
       - Check if User Type has changed
       - If any field has changed, set USR-MODIFIED-YES flag
    4. If USR-MODIFIED-YES flag is set:
       - Execute CICS REWRITE command with the following parameters:
         - DATASET: WS-USRSEC-FILE
         - FROM: SEC-USER-DATA
         - LENGTH: LENGTH OF SEC-USER-DATA
       - Check the response code (WS-RESP-CD):
         - If NORMAL:
           - Clear WS-MESSAGE
           - Set ERRMSGC to DFHGREEN (green color)
           - Construct success message: "User [SEC-USR-ID] has been updated ..."
           - Call SEND-USRUPD-SCREEN subroutine
         - If NOTFND:
           - Set WS-ERR-FLG to 'Y'
           - Set WS-MESSAGE to 'User ID NOT found...'
           - Set cursor to USRIDINL field
           - Call SEND-USRUPD-SCREEN subroutine
         - For any other response:
           - Display error codes (WS-RESP-CD and WS-REAS-CD)
           - Set WS-ERR-FLG to 'Y'
           - Set WS-MESSAGE to 'Unable to Update User...'
           - Set cursor to FNAMEL field
           - Call SEND-USRUPD-SCREEN subroutine
    5. If USR-MODIFIED-YES flag is not set:
       - Set WS-MESSAGE to 'Please modify to update ...'
       - Set ERRMSGC to DFHRED (red color)
       - Call SEND-USRUPD-SCREEN subroutine
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
```markdown
- ## Rule / Function / Method - Name: SEND-USRUPD-SCREEN

    - Detailed Description of the Rule:
      This rule sends the user update screen to the terminal, handling various scenarios including initial display, error messages, and data population.
    - What it proposes to do:
      Display the user interface for updating user information, including pre-populated data and error messages when applicable.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. Call POPULATE-HEADER-INFO subroutine to set up the screen header with current date, time, transaction ID, and program name.
    2. Move WS-MESSAGE to ERRMSGO field of COUSR2AO.
    3. If WS-ERR-FLG is 'Y', set ERRMSGC of COUSR2AO to DFHRED (red color).
    4. If updating a user (after PF5 key):
       - If update was successful, set ERRMSGC of COUSR2AO to DFHGREEN (green color).
       - Set WS-MESSAGE to "User [SEC-USR-ID] has been updated ...".
    5. If no modifications were made to user data:
       - Set WS-MESSAGE to "Please modify to update ...".
       - Set ERRMSGC of COUSR2AO to DFHRED.
    6. If displaying user data after ENTER key:
       - Set WS-MESSAGE to "Press PF5 key to save your updates ...".
       - Set ERRMSGC of COUSR2AO to DFHNEUTR (neutral color).
    7. Execute CICS SEND command with the following parameters:
       - MAP: 'COUSR2A'
       - MAPSET: 'COUSR02'
       - FROM: COUSR2AO
       - ERASE
       - CURSOR
    8. The cursor position (-1) is set to different fields based on the context:
       - USRIDINL: When User ID is empty or not found
       - FNAMEL: When First Name is empty or there's a general error
       - LNAMEL: When Last Name is empty
       - PASSWDL: When Password is empty
       - USRTYPEL: When User Type is empty
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 6
```markdown
- ## Rule / Function / Method - Name: POPULATE-HEADER-INFO

    - Detailed Description of the Rule:
      This rule populates the header information on the user update screen.
    - What it proposes to do:
      Fill in the current date, time, and other standard header fields.

    ## Rule Status: 
    - Relevant for Modernization

    ## Algorithm
    1. Get the current date and time using FUNCTION CURRENT-DATE
    2. Move CCDA-TITLE01 to TITLE01O of COUSR2AO
    3. Move CCDA-TITLE02 to TITLE02O of COUSR2AO
    4. Move WS-TRANID to TRNNAMEO of COUSR2AO
    5. Move WS-PGMNAME to PGMNAMEO of COUSR2AO
    6. Format the current date:
       - Extract month from WS-CURDATE-MONTH and move to WS-CURDATE-MM
       - Extract day from WS-CURDATE-DAY and move to WS-CURDATE-DD
       - Extract last two digits of year from WS-CURDATE-YEAR(3:2) and move to WS-CURDATE-YY
       - Construct WS-CURDATE-MM-DD-YY
       - Move WS-CURDATE-MM-DD-YY to CURDATEO of COUSR2AO
    7. Format the current time:
       - Extract hours from WS-CURTIME-HOURS and move to WS-CURTIME-HH
       - Extract minutes from WS-CURTIME-MINUTE and move to WS-CURTIME-MM
       - Extract seconds from WS-CURTIME-SECOND and move to WS-CURTIME-SS
       - Construct WS-CURTIME-HH-MM-SS
       - Move WS-CURTIME-HH-MM-SS to CURTIMEO of COUSR2AO
```

<!--rule-end-->