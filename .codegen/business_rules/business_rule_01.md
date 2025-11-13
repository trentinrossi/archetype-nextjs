# COADM01C.cbl: Admin Menu for Admin Users

## Overview
This CICS COBOL program, COADM01C, is part of the CardDemo application. It serves as the Admin Menu for administrative users. The program displays a menu of administrative options and handles user interactions with the menu. It acts as a central hub for various administrative functions within the CardDemo system.

<!-- general-rule-start -->
## General Business Rules:

1. User Authentication:
   - The program assumes that the user has already been authenticated as an admin user before reaching this menu.
   - If there's no communication area (COMMAREA) passed to the program, it returns to the sign-on screen (COSGN00C).

2. Menu Display:
   - The program displays a menu of administrative options to the user.
   - The menu options are defined in the COADM02Y copybook.
   - The program can display up to 10 menu options (OPTN001O to OPTN010O).

3. User Interaction:
   - The user can select an option by entering a number.
   - The program validates the user's input to ensure it's a valid option number.

4. Navigation:
   - When a valid option is selected, the program transfers control to the corresponding program for that option.
   - If the user presses PF3, the program returns to the sign-on screen.

5. Error Handling:
   - If an invalid key is pressed, an error message is displayed.
   - If an invalid option number is entered, an error message is shown.

6. Screen Information:
   - The screen displays header information including the current date and time.
   - The transaction ID (CA00) and program name (COADM01C) are shown on the screen.

7. Menu Options:
   - The menu options are dynamically built from the CDEMO-ADMIN-OPTIONS data structure.
   - Each option displays a number and a description.

8. Program Flow:
   - On initial entry, the program displays the menu screen.
   - On subsequent entries (after user input), it processes the user's selection.

9. Dummy Options:
   - If a selected option's program name starts with 'DUMMY', a "coming soon" message is displayed instead of transferring control.

10. Screen Handling:
    - The program uses BMS (Basic Mapping Support) for screen handling.
    - It uses the COADM1A map in the COADM01 mapset for input and output.

This program plays a crucial role in the CardDemo application by providing a centralized interface for administrative functions. It ensures that only authenticated admin users can access these functions and provides a user-friendly menu system for navigation between different administrative tasks.
<!-- general-rule-end -->
## Dependencies

This program relies on several copybooks and external resources for its functionality. These dependencies are crucial for the program's operation and will need to be considered in any modernization efforts.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| COCOM01Y | Common copybook | Copybook | COCOM01Y | May contain shared data structures or constants used across multiple programs |
| COADM02Y | Admin menu options | Copybook | COADM02Y | Contains the definitions for admin menu options, crucial for menu functionality |
| COADM01 | BMS map copybook | Copybook | COADM01 | Defines the screen layout for the admin menu, important for UI modernization |
| COTTL01Y | Title copybook | Copybook | COTTL01Y | Contains title information, may be used across multiple screens |
| CSDAT01Y | Date-related copybook | Copybook | CSDAT01Y | Likely contains date formatting or processing routines |
| CSMSG01Y | Message copybook | Copybook | CSMSG01Y | Contains message definitions, important for user communication |
| CSUSR01Y | User-related copybook | Copybook | CSUSR01Y | May contain user data structures or processing routines |
| DFHAID | CICS AID keys | Copybook | DFHAID | Standard CICS copybook for handling attention identifier keys |
| DFHBMSCA | CICS BMS attributes | Copybook | DFHBMSCA | Standard CICS copybook for BMS screen attributes |
| CICS | CICS runtime | External System | N/A | The program relies on CICS commands and environment |
| COSGN00C | Sign-on program | Program | COSGN00C | Referenced for returning to the sign-on screen |
| Admin option programs | Various admin programs | Programs | CDEMO-ADMIN-OPT-PGMNAME | The programs called for each admin option |

These dependencies are critical for the functioning of the admin menu. In a modernization effort, special attention should be given to:

1. The BMS map (COADM01) which defines the screen layout and may need to be converted to a modern UI framework.
2. The CICS commands and environment, which may need to be replaced or emulated in a new platform.
3. The various copybooks, which may contain shared business logic or data structures that need to be carefully migrated.
4. The interaction with other programs (like COSGN00C and the admin option programs), which will need to be maintained in the modernized system.
## Detailed Rules
## Data Structure

This section describes the key data structures used in the COADM01C program. These structures are crucial for the program's functionality and will be important considerations in any modernization efforts.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| WS-PGMNAME | Program Name | Alphanumeric | 8 | Stores the current program name | Yes | 'COADM01C' | Used for logging and screen display | Yes | Control |
| WS-TRANID | Transaction ID | Alphanumeric | 4 | Stores the transaction ID | Yes | 'CA00' | Used for CICS communication | Yes | Control |
| WS-MESSAGE | Message | Alphanumeric | 80 | Stores messages to be displayed to the user | No | SPACES | Used for user feedback | Yes | Display |
| WS-USRSEC-FILE | User Security File | Alphanumeric | 8 | Name of the user security file | Yes | 'USRSEC' | May be used for user validation | Yes | Control |
| WS-ERR-FLG | Error Flag | Alphanumeric | 1 | Indicates if an error has occurred | Yes | 'N' | 'Y' for error, 'N' for no error | Yes | Control |
| WS-RESP-CD | Response Code | Numeric | 9 | Stores CICS response codes | No | ZEROS | Used for error handling | Yes | Control |
| WS-REAS-CD | Reason Code | Numeric | 9 | Stores CICS reason codes | No | ZEROS | Used for error handling | Yes | Control |
| WS-OPTION-X | Option (String) | Alphanumeric | 2 | Stores user's menu selection as string | No | SPACES | Intermediate storage before conversion | Yes | Input |
| WS-OPTION | Option (Numeric) | Numeric | 2 | Stores user's menu selection as number | No | 0 | Used to determine selected admin function | Yes | Control |
| WS-IDX | Index | Numeric | 4 | Loop counter for building menu options | No | ZEROS | Used in BUILD-MENU-OPTIONS | Yes | Control |
| WS-ADMIN-OPT-TXT | Admin Option Text | Alphanumeric | 40 | Temporary storage for building menu option text | No | SPACES | Used in BUILD-MENU-OPTIONS | Yes | Temporary |
| CDEMO-ADMIN-OPT-COUNT | Admin Option Count | Numeric | 2 | Number of available admin options | Yes | 4 | Defined in COADM02Y | Yes | Control |
| CDEMO-ADMIN-OPT-NUM | Admin Option Number | Numeric | 2 | Number of each admin option | Yes | N/A | Part of CDEMO-ADMIN-OPTIONS-DATA | Yes | Display |
| CDEMO-ADMIN-OPT-NAME | Admin Option Name | Alphanumeric | 35 | Name/description of each admin option | Yes | N/A | Part of CDEMO-ADMIN-OPTIONS-DATA | Yes | Display |
| CDEMO-ADMIN-OPT-PGMNAME | Admin Option Program Name | Alphanumeric | 8 | Program name for each admin option | Yes | N/A | Part of CDEMO-ADMIN-OPTIONS-DATA | Yes | Control |
| TRNNAMEO | Transaction Name Output | Alphanumeric | 4 | Transaction name displayed on screen | Yes | N/A | Part of COADM1AO | Yes | Display |
| PGMNAMEO | Program Name Output | Alphanumeric | 8 | Program name displayed on screen | Yes | N/A | Part of COADM1AO | Yes | Display |
| CURDATEO | Current Date Output | Alphanumeric | 8 | Current date displayed on screen | Yes | N/A | Part of COADM1AO | Yes | Display |
| CURTIMEO | Current Time Output | Alphanumeric | 8 | Current time displayed on screen | Yes | N/A | Part of COADM1AO | Yes | Display |
| OPTIONI | Option Input | Alphanumeric | 2 | User's menu selection input | No | N/A | Part of COADM1AI | Yes | Input |
| OPTIONO | Option Output | Alphanumeric | 2 | User's menu selection displayed | No | N/A | Part of COADM1AO | Yes | Display |
| ERRMSGO | Error Message Output | Alphanumeric | 78 | Error message displayed on screen | No | N/A | Part of COADM1AO | Yes | Display |



<!--rule-start-->
## Reviewed Rule 1
```markdown
- ## Rule / Function / Method - Main Program Flow:
   - Detailed Description of the Rule:
     This rule governs the main flow of the COADM01C program, which handles the Admin Menu for administrative users in the CardDemo application.
   - What it proposes to do:
     Control the program's execution flow, including initial screen display, user input processing, and navigation between different administrative functions.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Initialize variables:
      - Set ERR-FLG-OFF to TRUE
      - Clear WS-MESSAGE and ERRMSGO of COADM1AO

   2. Check if EIBCALEN (CICS Communication Area Length) is 0:
      - If yes, set CDEMO-FROM-PROGRAM to 'COSGN00C' and perform RETURN-TO-SIGNON-SCREEN
      - If no, continue to step 3

   3. Move DFHCOMMAREA to CARDDEMO-COMMAREA

   4. Check if CDEMO-PGM-REENTER is FALSE:
      - If yes:
        a. Set CDEMO-PGM-REENTER to TRUE
        b. Clear COADM1AO (set to LOW-VALUES)
        c. Perform SEND-MENU-SCREEN
      - If no, continue to step 5

   5. Perform RECEIVE-MENU-SCREEN

   6. Evaluate EIBAID (Attention Identifier):
      - If DFHENTER:
        Perform PROCESS-ENTER-KEY
      - If DFHPF3:
        a. Set CDEMO-TO-PROGRAM to 'COSGN00C'
        b. Perform RETURN-TO-SIGNON-SCREEN
      - For any other key:
        a. Set WS-ERR-FLG to 'Y'
        b. Set WS-MESSAGE to CCDA-MSG-INVALID-KEY
        c. Perform SEND-MENU-SCREEN

   7. Return to CICS, passing WS-TRANID and CARDDEMO-COMMAREA

   8. PROCESS-ENTER-KEY:
      a. Extract and validate the user's option input:
         - Remove trailing spaces from OPTIONI of COADM1AI
         - Move the cleaned input to WS-OPTION-X
         - Replace any remaining spaces with '0'
         - Convert WS-OPTION-X to numeric in WS-OPTION
         - Move WS-OPTION to OPTIONO of COADM1AO
      b. Validate the option:
         - If WS-OPTION is not numeric, or
         - If WS-OPTION > CDEMO-ADMIN-OPT-COUNT, or
         - If WS-OPTION = ZEROS
         Then:
           - Set WS-ERR-FLG to 'Y'
           - Set WS-MESSAGE to 'Please enter a valid option number...'
           - Perform SEND-MENU-SCREEN
      c. If no error:
         - If CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION) does not start with 'DUMMY':
           * Set CDEMO-FROM-TRANID to WS-TRANID
           * Set CDEMO-FROM-PROGRAM to WS-PGMNAME
           * Set CDEMO-PGM-CONTEXT to ZEROS
           * Transfer control to the program specified in CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION)
         - Otherwise:
           * Set WS-MESSAGE to 'This option is coming soon ...'
           * Set ERRMSGC of COADM1AO to DFHGREEN
           * Perform SEND-MENU-SCREEN

   9. SEND-MENU-SCREEN:
      a. Perform POPULATE-HEADER-INFO
      b. Perform BUILD-MENU-OPTIONS
      c. Move WS-MESSAGE to ERRMSGO of COADM1AO
      d. Send the COADM1A map to the screen

   10. POPULATE-HEADER-INFO:
       a. Get the current date and time
       b. Populate the screen header fields (TITLE01O, TITLE02O, TRNNAMEO, PGMNAMEO, CURDATEO, CURTIMEO)

   11. BUILD-MENU-OPTIONS:
       a. For each option (1 to CDEMO-ADMIN-OPT-COUNT):
          - Construct the option text using CDEMO-ADMIN-OPT-NUM and CDEMO-ADMIN-OPT-NAME
          - Move the constructed text to the corresponding OPTNxxxO field (OPTN001O to OPTN010O)
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
- ## Rule / Function / Method - PROCESS-ENTER-KEY:
   - Detailed Description of the Rule:
     This rule processes the user's input when the Enter key is pressed on the Admin Menu screen.
   - What it proposes to do:
     Validate the user's option selection and either navigate to the selected admin function or display an error message.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Extract the user's option selection:
      a. Start from the rightmost character of OPTIONI in COADM1AI
      b. Move left until a non-space character is found or the start of the field is reached
      c. Copy the non-space characters to WS-OPTION-X
      d. Replace any remaining spaces in WS-OPTION-X with '0'
      e. Convert WS-OPTION-X to a numeric value in WS-OPTION
      f. Move WS-OPTION to OPTIONO of COADM1AO

   2. Validate the user's option:
      - If WS-OPTION is not numeric, OR
      - If WS-OPTION is greater than CDEMO-ADMIN-OPT-COUNT, OR
      - If WS-OPTION is zero:
        a. Set WS-ERR-FLG to 'Y'
        b. Set WS-MESSAGE to 'Please enter a valid option number...'
        c. Perform SEND-MENU-SCREEN
        d. Exit the routine

   3. If no error flag is set:
      a. Check if CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION) does not start with 'DUMMY':
         - If true:
           i. Set CDEMO-FROM-TRANID to WS-TRANID
           ii. Set CDEMO-FROM-PROGRAM to WS-PGMNAME
           iii. Set CDEMO-PGM-CONTEXT to zeros
           iv. Transfer control to the program specified in CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION), passing CARDDEMO-COMMAREA
      b. If the program name starts with 'DUMMY':
         i. Clear WS-MESSAGE
         ii. Set ERRMSGC of COADM1AO to DFHGREEN
         iii. Construct a message: "This option is coming soon ..."
         iv. Perform SEND-MENU-SCREEN

   4. If an error flag is set or after processing a 'DUMMY' option:
      a. Return to the calling program, passing CARDDEMO-COMMAREA and setting the transaction ID to WS-TRANID

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
- ## Rule / Function / Method - RETURN-TO-SIGNON-SCREEN:
   - Detailed Description of the Rule:
     This rule handles the return to the sign-on screen when required.
   - What it proposes to do:
     Ensure that control is transferred back to the sign-on program (COSGN00C) when needed.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Check if CDEMO-TO-PROGRAM is LOW-VALUES or SPACES:
      - If yes, set CDEMO-TO-PROGRAM to 'COSGN00C'

   2. Transfer control to the program specified in CDEMO-TO-PROGRAM using CICS XCTL command
      - The XCTL command is executed without passing any COMMAREA

   3. This rule is called in two scenarios:
      - When EIBCALEN is 0 (no COMMAREA passed):
        - Set CDEMO-FROM-PROGRAM to 'COSGN00C' before calling this rule
      - When EIBAID is DFHPF3 (PF3 key pressed):
        - Set CDEMO-TO-PROGRAM to 'COSGN00C' before calling this rule

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
```markdown
- ## Rule / Function / Method - SEND-MENU-SCREEN:
   - Detailed Description of the Rule:
     This rule prepares and sends the Admin Menu screen to the user.
   - What it proposes to do:
     Populate the screen with current information and menu options, then send it to the user's terminal.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Perform POPULATE-HEADER-INFO to set up the screen header:
      - Move CCDA-TITLE01 to TITLE01O of COADM1AO
      - Move CCDA-TITLE02 to TITLE02O of COADM1AO
      - Move WS-TRANID to TRNNAMEO of COADM1AO
      - Move WS-PGMNAME to PGMNAMEO of COADM1AO
      - Get current date and time using FUNCTION CURRENT-DATE
      - Format and move current date (MM/DD/YY) to CURDATEO of COADM1AO
      - Format and move current time (HH:MM:SS) to CURTIMEO of COADM1AO

   2. Perform BUILD-MENU-OPTIONS to populate the menu options:
      - Loop through CDEMO-ADMIN-OPT-COUNT (up to 10 options)
      - For each option, create a string with format: "[option number]. [option name]"
      - Move each option string to the corresponding field (OPTN001O to OPTN010O) based on its index

   3. Move WS-MESSAGE to ERRMSGO of COADM1AO

   4. Send the screen to the user's terminal:
      - Use CICS SEND command
      - Specify MAP as 'COADM1A'
      - Specify MAPSET as 'COADM01'
      - Send data FROM COADM1AO
      - Use the ERASE option to clear the screen before displaying new content
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
- ## Rule / Function / Method - RECEIVE-MENU-SCREEN:
   - Detailed Description of the Rule:
     This rule receives user input from the Admin Menu screen.
   - What it proposes to do:
     Capture the user's input from the screen for further processing.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Use CICS RECEIVE command to get user input:
      - Specify MAP as 'COADM1A'
      - Specify MAPSET as 'COADM01'
      - Receive data INTO COADM1AI
      - Store the response code in WS-RESP-CD
      - Store the reason code in WS-REAS-CD
   2. The CICS RECEIVE command is executed with the following parameters:
      - MAP('COADM1A')
      - MAPSET('COADM01')
      - INTO(COADM1AI)
      - RESP(WS-RESP-CD)
      - RESP2(WS-REAS-CD)
   3. After execution, the user input will be stored in the COADM1AI structure
   4. Any errors or exceptional conditions during the receive operation will be reflected in the WS-RESP-CD and WS-REAS-CD fields

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 6
```markdown
- ## Rule / Function / Method - POPULATE-HEADER-INFO:
   - Detailed Description of the Rule:
     This rule populates the header information for the Admin Menu screen.
   - What it proposes to do:
     Fill in the screen's header with current date, time, and other relevant information.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Get the current date and time:
      - Use FUNCTION CURRENT-DATE and store in WS-CURDATE-DATA

   2. Populate screen titles:
      - Move CCDA-TITLE01 to TITLE01O of COADM1AO
      - Move CCDA-TITLE02 to TITLE02O of COADM1AO

   3. Set transaction and program names:
      - Move WS-TRANID to TRNNAMEO of COADM1AO
      - Move WS-PGMNAME to PGMNAMEO of COADM1AO

   4. Format and set the current date:
      - Extract month from WS-CURDATE-MONTH
      - Extract day from WS-CURDATE-DAY
      - Extract year (last 2 digits) from WS-CURDATE-YEAR
      - Combine as MM-DD-YY format
      - Move the formatted date to CURDATEO of COADM1AO

   5. Format and set the current time:
      - Extract hours from WS-CURTIME-HOURS
      - Extract minutes from WS-CURTIME-MINUTE
      - Extract seconds from WS-CURTIME-SECOND
      - Combine as HH:MM:SS format
      - Move the formatted time to CURTIMEO of COADM1AO

   6. Specific field movements:
      - Move WS-CURDATE-MONTH to WS-CURDATE-MM
      - Move WS-CURDATE-DAY to WS-CURDATE-DD
      - Move WS-CURDATE-YEAR(3:2) to WS-CURDATE-YY
      - Move WS-CURDATE-MM-DD-YY to CURDATEO of COADM1AO
      - Move WS-CURTIME-HOURS to WS-CURTIME-HH
      - Move WS-CURTIME-MINUTE to WS-CURTIME-MM
      - Move WS-CURTIME-SECOND to WS-CURTIME-SS
      - Move WS-CURTIME-HH-MM-SS to CURTIMEO of COADM1AO
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 7
- ## Rule / Function / Method - BUILD-MENU-OPTIONS:
   - Detailed Description of the Rule:
     This rule constructs the menu options for display on the Admin Menu screen.
   - What it proposes to do:
     Dynamically build the list of administrative options available to the user.

   ## Rule Status: 
   - Relevant for Modernization

   ## Algorithm
   1. Initialize a loop counter WS-IDX to 1

   2. For each option (while WS-IDX <= CDEMO-ADMIN-OPT-COUNT):
      a. Clear WS-ADMIN-OPT-TXT by setting it to SPACES
      b. Construct the option text:
         - Concatenate CDEMO-ADMIN-OPT-NUM(WS-IDX), '. ', and CDEMO-ADMIN-OPT-NAME(WS-IDX)
         - Store the result in WS-ADMIN-OPT-TXT
      c. Based on the value of WS-IDX, move WS-ADMIN-OPT-TXT to the corresponding screen field:
         - If WS-IDX = 1, move to OPTN001O
         - If WS-IDX = 2, move to OPTN002O
         - If WS-IDX = 3, move to OPTN003O
         - If WS-IDX = 4, move to OPTN004O
         - If WS-IDX = 5, move to OPTN005O
         - If WS-IDX = 6, move to OPTN006O
         - If WS-IDX = 7, move to OPTN007O
         - If WS-IDX = 8, move to OPTN008O
         - If WS-IDX = 9, move to OPTN009O
         - If WS-IDX = 10, move to OPTN010O
         - If WS-IDX > 10, no action is taken (CONTINUE)
      d. Increment WS-IDX by 1
      e. Repeat from step 2 if WS-IDX <= CDEMO-ADMIN-OPT-COUNT

   3. The process ends when all options have been processed (WS-IDX > CDEMO-ADMIN-OPT-COUNT)

<!--rule-end-->