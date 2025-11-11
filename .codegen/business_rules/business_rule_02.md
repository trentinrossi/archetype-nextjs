# CBACT02C.cbl: Card Data File Reader and Printer

## Overview
This COBOL program, CBACT02C, is part of the CardDemo application. It is a batch program designed to read and print card data from a VSAM KSDS (Key Sequenced Data Set) file. The program sequentially reads records from the card file and displays them, providing a simple way to view the contents of the card data file.

The main functions of this program include:
1. Opening the card data file
2. Reading records sequentially from the file
3. Displaying each record
4. Handling file operation errors
5. Closing the file after processing

This program plays a crucial role in data verification and auditing processes within the CardDemo application by allowing users to view the raw card data stored in the system.

<!-- general-rule-start -->
## General Business Rules:

1. File Handling:
   - The program uses a VSAM KSDS file named CARDFILE to store card data.
   - The file is opened in INPUT mode, meaning it's read-only.
   - Records are read sequentially from the beginning of the file to the end.

2. Record Structure:
   - Each record in the CARDFILE consists of two parts:
     a. FD-CARD-NUM (16 characters): This is the key field, likely representing the card number.
     b. FD-CARD-DATA (134 characters): This contains additional card-related information.

3. Processing Logic:
   - The program reads records one by one until it reaches the end of the file.
   - Each record read is displayed immediately, allowing for a complete view of the file contents.

4. Error Handling:
   - The program includes comprehensive error checking for file operations (open, read, close).
   - If an error occurs during any file operation, the program displays an error message, shows the file status, and then abends (abnormally ends) with a code of 999.

5. End-of-File Handling:
   - The program uses a flag (END-OF-FILE) to determine when it has reached the end of the CARDFILE.
   - When the end of file is reached (file status '10'), the program stops reading and proceeds to close the file.

6. Performance Considerations:
   - As the program reads and displays every record in the file, it may not be suitable for very large datasets without modification.

7. Security Implications:
   - This program displays all card data, including potentially sensitive information. Usage should be restricted to authorized personnel only.

8. Audit Trail:
   - The program provides a simple form of audit trail by displaying start and end messages, which can be captured in job logs for verification of program execution.

This program serves as a crucial tool for data verification, troubleshooting, and auditing in the CardDemo application. It allows system administrators or auditors to view the raw contents of the card data file, which can be essential for ensuring data integrity, investigating discrepancies, or performing system checks.
<!-- general-rule-end -->
## Dependencies

This program relies on several external dependencies for its operation. These dependencies are crucial for the program's functionality and must be considered in any modernization efforts.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| CARDFILE | VSAM KSDS file containing card data | File | CARDFILE | High - Data storage mechanism may need to be updated |
| CVACT02Y | Copybook containing card record structure | Copybook | COPY CVACT02Y | High - Data structure definition, may need adjustment |
| CEE3ABD | IBM Language Environment service for abnormal termination | External Routine | CALL 'CEE3ABD' | Medium - Error handling might be implemented differently |
| VSAM | Virtual Storage Access Method for file handling | File System | N/A | High - Modern systems might use different file systems or databases |

The program's primary dependency is the CARDFILE, which is a VSAM KSDS file. This file system is specific to mainframe environments and would likely need to be replaced with a more modern database or file system in a modernized version of the application.

The CVACT02Y copybook defines the structure of the card records. In a modernization effort, this data structure might need to be adjusted to align with new storage mechanisms or expanded data requirements.

The use of CEE3ABD for abnormal termination is specific to the IBM Language Environment. In a modernized system, different error handling and logging mechanisms might be more appropriate.

The reliance on VSAM for file handling is a significant consideration for modernization. Modern systems typically use relational databases or other types of file systems, which would require substantial changes to the data access logic in this program.
## Detailed Rules
## Data Structure

This section describes the data structure used in the CBACT02C program, focusing on the key fields and variables that are crucial for the program's operation.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| 1 | FD-CARD-NUM | Alphanumeric | 16 | Card number, serves as the record key | Yes | None | Part of CARDFILE record | High | Sensitive |
| 2 | FD-CARD-DATA | Alphanumeric | 134 | Additional card data | Yes | None | Part of CARDFILE record | High | Sensitive |
| 3 | CARDFILE-STATUS | Alphanumeric | 2 | File status for CARDFILE operations | Yes | None | Used for error checking | Medium | Technical |
| 4 | END-OF-FILE | Alphanumeric | 1 | Flag indicating end of file | Yes | 'N' | Controls main processing loop | Medium | Control |
| 5 | APPL-RESULT | Numeric | 9 (Binary) | Application result code | Yes | None | Used for error handling | Medium | Control |
| 6 | IO-STATUS | Alphanumeric | 2 | General I/O status | Yes | None | Used for error reporting | Medium | Technical |
| 7 | ABCODE | Numeric | 9 (Binary) | Abend code | No | None | Set to 999 for abnormal termination | Low | Technical |
| 8 | TIMING | Numeric | 9 (Binary) | Timing variable | No | None | Set to 0 before abend | Low | Technical |
| 9 | IO-STATUS-04 | Alphanumeric | 4 | Formatted I/O status for display | No | None | Used in error reporting | Low | Technical |
| 10 | CARD-RECORD | Structure | Varies | Complete card record structure | Yes | None | Defined in CVACT02Y copybook | High | Composite |

Note: The CARD-RECORD structure is defined in the CVACT02Y copybook, which is not fully detailed in the provided code. It likely contains additional fields that make up the complete card record, including the FD-CARD-NUM and FD-CARD-DATA.



<!--rule-start-->
## Reviewed Rule 1
```markdown
- ## Rule / Function / Method - Name: Open CARDFILE

- Detailed Description of the Rule:
  This rule opens the CARDFILE for input operations. It ensures that the file is available and ready for reading. The CARDFILE is an indexed file with sequential access mode.

- What it proposes to do:
  Prepare the CARDFILE for sequential reading of card records.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Set APPL-RESULT to 8 (initial error state)
2. Execute OPEN INPUT operation on CARDFILE-FILE
3. Check CARDFILE-STATUS:
   - If CARDFILE-STATUS is '00' (successful open):
     - Set APPL-RESULT to 0 (success)
   - Else:
     - Set APPL-RESULT to 12 (error state)
4. If APPL-RESULT is 0:
   - Continue processing
5. Else:
   - Display 'ERROR OPENING CARDFILE'
   - Move CARDFILE-STATUS to IO-STATUS
   - Perform 9910-DISPLAY-IO-STATUS:
     - If IO-STATUS is not numeric OR IO-STAT1 is '9':
       - Move IO-STAT1 to IO-STATUS-04(1:1)
       - Move 0 to TWO-BYTES-BINARY
       - Move IO-STAT2 to TWO-BYTES-RIGHT
       - Move TWO-BYTES-BINARY to IO-STATUS-0403
       - Display 'FILE STATUS IS: NNNN' followed by IO-STATUS-04
     - Else:
       - Move '0000' to IO-STATUS-04
       - Move IO-STATUS to IO-STATUS-04(3:2)
       - Display 'FILE STATUS IS: NNNN' followed by IO-STATUS-04
   - Perform 9999-ABEND-PROGRAM:
     - Display 'ABENDING PROGRAM'
     - Set TIMING to 0
     - Set ABCODE to 999
     - Call 'CEE3ABD' to abort the program
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
```markdown
- ## Rule / Function / Method - Name: Read Next CARDFILE Record

- Detailed Description of the Rule:
  This rule reads the next sequential record from the CARDFILE and processes it.

- What it proposes to do:
  Retrieve the next card record from the file and prepare it for display or further processing.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Execute READ operation on CARDFILE-FILE, reading into CARD-RECORD
2. Check CARDFILE-STATUS:
   - If CARDFILE-STATUS is '00' (successful read):
     - Set APPL-RESULT to 0 (success)
   - Else If CARDFILE-STATUS is '10' (end of file):
     - Set APPL-RESULT to 16 (EOF indicator)
   - Else:
     - Set APPL-RESULT to 12 (error state)
3. If APPL-RESULT is 0:
   - Continue processing (record is available in CARD-RECORD)
4. Else If APPL-RESULT is 16:
   - Set END-OF-FILE to 'Y'
5. Else:
   - Display 'ERROR READING CARDFILE'
   - Move CARDFILE-STATUS to IO-STATUS
   - Perform 9910-DISPLAY-IO-STATUS
     - If IO-STATUS is not numeric OR first digit is '9':
       - Move first digit of IO-STATUS to IO-STATUS-04(1:1)
       - Move second digit of IO-STATUS to TWO-BYTES-RIGHT
       - Convert TWO-BYTES-BINARY to IO-STATUS-0403
       - Display 'FILE STATUS IS: NNNN' followed by IO-STATUS-04
     - Else:
       - Move '0000' to IO-STATUS-04
       - Move IO-STATUS to IO-STATUS-04(3:2)
       - Display 'FILE STATUS IS: NNNN' followed by IO-STATUS-04
   - Perform 9999-ABEND-PROGRAM
     - Display 'ABENDING PROGRAM'
     - Set TIMING to 0
     - Set ABCODE to 999
     - Call 'CEE3ABD' to abort the program
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
```markdown
- ## Rule / Function / Method - Name: Display Card Record

- Detailed Description of the Rule:
  This rule reads card records from a VSAM KSDS file and displays the contents of each card record until the end of the file is reached.

- What it proposes to do:
  Read card records sequentially from the CARDFILE, display each record's content, and handle any file operation errors.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Open the CARDFILE for input.
   - If open is successful (CARDFILE-STATUS = '00'), continue.
   - If open fails, display 'ERROR OPENING CARDFILE', show the file status, and abend the program with code 999.

2. Perform the following steps until END-OF-FILE = 'Y':
   a. Read the next record from CARDFILE into CARD-RECORD.
   b. Check the CARDFILE-STATUS:
      - If '00' (successful read):
        - Set APPL-RESULT to 0.
        - Display the entire CARD-RECORD.
      - If '10' (end of file reached):
        - Set APPL-RESULT to 16.
        - Set END-OF-FILE to 'Y'.
      - For any other status:
        - Set APPL-RESULT to 12.
        - Display 'ERROR READING CARDFILE'.
        - Show the file status.
        - Abend the program with code 999.

3. After all records are processed or in case of an error, close the CARDFILE.
   - If close is successful (CARDFILE-STATUS = '00'), continue.
   - If close fails, display 'ERROR CLOSING CARDFILE', show the file status, and abend the program with code 999.

Note: CARD-RECORD structure is defined in the CVACT02Y copybook and includes:
  - Card number (16 characters)
  - Additional card data (134 characters)

Additional Information:
- The program displays 'START OF EXECUTION OF PROGRAM CBACT02C' at the beginning.
- The program displays 'END OF EXECUTION OF PROGRAM CBACT02C' before terminating normally.
- Any abend situation uses the 'CEE3ABD' call to terminate the program.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
- ## Rule / Function / Method - Name: Close CARDFILE

- Detailed Description of the Rule:
  This rule closes the CARDFILE after all records have been processed.

- What it proposes to do:
  Properly close the file to release system resources and ensure data integrity.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Set APPL-RESULT to 8 (initial error state)
2. Execute CLOSE operation on CARDFILE-FILE
3. Check CARDFILE-STATUS:
   - If CARDFILE-STATUS is '00' (successful close):
     - Set APPL-RESULT to 0 (success)
   - Else:
     - Set APPL-RESULT to 12 (error state)
4. If APPL-RESULT is 0:
   - Continue processing
5. Else:
   - Display 'ERROR CLOSING CARDFILE'
   - Move CARDFILE-STATUS to IO-STATUS
   - Perform 9910-DISPLAY-IO-STATUS
     - If IO-STATUS is not numeric OR IO-STAT1 is '9':
       - Move IO-STAT1 to IO-STATUS-04(1:1)
       - Move 0 to TWO-BYTES-BINARY
       - Move IO-STAT2 to TWO-BYTES-RIGHT
       - Move TWO-BYTES-BINARY to IO-STATUS-0403
       - Display 'FILE STATUS IS: NNNN' followed by IO-STATUS-04
     - Else:
       - Move '0000' to IO-STATUS-04
       - Move IO-STATUS to IO-STATUS-04(3:2)
       - Display 'FILE STATUS IS: NNNN' followed by IO-STATUS-04
   - Perform 9999-ABEND-PROGRAM
     - Display 'ABENDING PROGRAM'
     - Set TIMING to 0
     - Set ABCODE to 999
     - Call 'CEE3ABD' to abort the program

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
```markdown
- ## Rule / Function / Method - Name: Display IO Status

- Detailed Description of the Rule:
  This rule formats and displays the IO status for error reporting.

- What it proposes to do:
  Provide detailed file status information in case of file operation errors.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Check if IO-STATUS is not numeric OR if IO-STAT1 is '9':
   - Move IO-STAT1 to IO-STATUS-04(1:1)
   - Set TWO-BYTES-BINARY to 0
   - Move IO-STAT2 to TWO-BYTES-RIGHT
   - Move TWO-BYTES-BINARY to IO-STATUS-0403
   - Display 'FILE STATUS IS: NNNN' followed by IO-STATUS-04
2. Else:
   - Move '0000' to IO-STATUS-04
   - Move IO-STATUS to IO-STATUS-04(3:2)
   - Display 'FILE STATUS IS: NNNN' followed by IO-STATUS-04
3. Exit the procedure after displaying the file status.

Note: 
- IO-STATUS-04 is a 4-digit field where:
  - IO-STATUS-0401 is the first digit (initialized to 0)
  - IO-STATUS-0403 is the last three digits (initialized to 000)
- TWO-BYTES-BINARY is a 4-digit binary field that is redefined as TWO-BYTES-ALPHA, which consists of TWO-BYTES-LEFT and TWO-BYTES-RIGHT.
- This procedure is called when there's an error in opening, reading, or closing the CARDFILE.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 6
```markdown
- ## Rule / Function / Method - Name: Abend Program

- Detailed Description of the Rule:
  This rule handles abnormal termination of the program when unrecoverable errors occur during file operations or other critical processes.

- What it proposes to do:
  Properly terminate the program in case of unrecoverable errors, displaying relevant error information and initiating a controlled abnormal end.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Display the message 'ABENDING PROGRAM'
2. Set TIMING to 0
3. Set ABCODE to 999
4. Call 'CEE3ABD' to initiate abnormal termination
5. Before the abend, perform the following:
   - Display specific error messages based on the operation that failed (e.g., 'ERROR READING CARDFILE', 'ERROR OPENING CARDFILE', 'ERROR CLOSING CARDFILE')
   - Move the CARDFILE-STATUS to IO-STATUS
   - Perform 9910-DISPLAY-IO-STATUS to show detailed file status information:
     - If IO-STATUS is not numeric or IO-STAT1 is '9':
       - Format and display file status as 'FILE STATUS IS: NNNN' where the first N is IO-STAT1 and the last three N's are the binary representation of IO-STAT2
     - Otherwise:
       - Display file status as 'FILE STATUS IS: 00NN' where NN is the original IO-STATUS
6. The abend routine is called in the following scenarios:
   - When there's an error reading the CARDFILE (status not '00' or '10')
   - When there's an error opening the CARDFILE (status not '00')
   - When there's an error closing the CARDFILE (status not '00')
```

<!--rule-end-->