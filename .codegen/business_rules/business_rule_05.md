# CBCUS01C.cbl: Customer Data File Reader and Printer

## Overview
This COBOL program, CBCUS01C, is part of the CardDemo application. It is a batch program designed to read and print customer data from an indexed file. The program sequentially reads customer records from the file and displays them, providing a simple way to view the contents of the customer database.

<!-- general-rule-start -->
## General Business Rules:
1. File Handling:
   - The program opens an indexed file named CUSTFILE, which contains customer records.
   - It uses sequential access mode to read through all the records in the file.
   - The file is organized with CUST-ID as the record key.

2. Record Processing:
   - The program reads each customer record from the file.
   - Each record is displayed immediately after being read.
   - This continues until the end of the file is reached.

3. Error Handling:
   - The program checks for file status after each file operation (open, read, close).
   - If any error occurs during file operations, the program displays an error message, shows the file status, and then abends.
   - Specific error handling is implemented for file opening, reading, and closing operations.

4. End of File Processing:
   - The program recognizes the end of file condition (status '10') and stops reading at that point.

5. Program Flow:
   - The program starts by opening the customer file.
   - It then enters a loop to read and display records until the end of file is reached.
   - After processing all records, it closes the file.
   - The program displays messages at the start and end of execution for clarity.

6. Data Structure:
   - The customer record structure is defined in a copybook named CVCUS01Y, which is included in the program.
   - This suggests that the customer data structure is standardized and potentially used across multiple programs in the CardDemo application.

7. Abend Handling:
   - In case of critical errors, the program uses a call to 'CEE3ABD' to abend, ensuring that the program terminates in a controlled manner and potentially triggering any system-level error handling or logging.

This program serves as a diagnostic or reporting tool, allowing users or administrators to view the raw contents of the customer database. It could be used for data verification, auditing, or troubleshooting purposes within the CardDemo application ecosystem.
<!-- general-rule-end -->
## Dependencies

This program relies on several external dependencies for its operation. These dependencies are crucial for the program's functionality and should be carefully considered in any modernization efforts.

| Dependency | Description | Type | Reference | Relevance for Modernization |
|-------------|-----------|------|------------|------------------------------|
| CUSTFILE | Indexed VSAM file containing customer records | File | CUSTFILE | High - Consider migrating to a modern database system |
| CVCUS01Y | Copybook containing customer record structure | Copybook | CVCUS01Y | Medium - May need to be updated if data structure changes |
| CEE3ABD | Language Environment service for abnormal termination | External Call | CEE3ABD | Low - Consider replacing with modern error handling techniques |
| VSAM | Virtual Storage Access Method for file handling | File System | N/A | High - May need to be replaced with modern file or database access methods |
| Language Environment | Runtime environment for COBOL programs | Runtime | N/A | Medium - Consider compatibility with modern COBOL compilers or runtime environments |

The program's primary dependency is the CUSTFILE, which is an indexed VSAM file. This file system is specific to mainframe environments and would be a key consideration in any modernization effort. The CVCUS01Y copybook defines the structure of the customer records, which is crucial for data interpretation and might need updates if the data structure changes during modernization.

The use of CEE3ABD for abnormal termination is a mainframe-specific feature that might need to be replaced with more modern error handling and logging mechanisms in a modernized system.

The program's reliance on VSAM for file handling is significant for modernization efforts. VSAM is a mainframe-specific technology, and moving to a more modern, platform-independent database or file system would be a major consideration in modernizing this application.

Finally, the program's dependency on the Language Environment runtime is important to consider when thinking about potential migration to different COBOL compilers or runtime environments in a modernized system.
## Detailed Rules
## Data Structure

This section describes the data structure used in the CBCUS01C program, focusing on the customer record structure and other relevant data elements.

| Field ID | Field Name | Data Type | Field Size | Description | Mandatory | Default Value | Additional Notes | Relevant for Modernization | Information Type |
|-------------|---------------|---------------|------------------|-----------|-------------|--------------|------------------|-----------------------------|--------------------|
| 1 | FD-CUST-ID | Numeric | 9 | Customer ID | Yes | None | Primary key for customer records | High | Identifier |
| 2 | FD-CUST-DATA | Alphanumeric | 491 | Customer data | Yes | None | Contains all other customer information | High | Composite |
| 3 | CUSTFILE-STATUS | Alphanumeric | 2 | File status code | Yes | None | Used for error handling | Medium | System |
| 4 | IO-STATUS | Alphanumeric | 2 | I/O operation status | Yes | None | Used for error handling and display | Medium | System |
| 5 | TWO-BYTES-BINARY | Binary | 4 | Binary representation of status | No | None | Used for status conversion | Low | System |
| 6 | IO-STATUS-04 | Numeric | 4 | Formatted I/O status | No | '0000' | Used for status display | Low | System |
| 7 | APPL-RESULT | Binary | 4 | Application result code | Yes | None | Used for flow control | Medium | System |
| 8 | END-OF-FILE | Alphanumeric | 1 | End of file indicator | Yes | 'N' | Controls main processing loop | Medium | Control |
| 9 | ABCODE | Binary | 4 | Abend code | No | None | Used in abnormal termination | Low | System |
| 10 | TIMING | Binary | 4 | Timing information | No | None | Not used in current implementation | Low | System |

Note: The detailed structure of FD-CUST-DATA is not provided in the given code snippet. It's likely defined in the CVCUS01Y copybook, which is not included in the provided source. This structure would be crucial for modernization efforts and should be analyzed in detail when available.



<!--rule-start-->
## Reviewed Rule 1
```markdown
- ## Rule / Function / Method - Open Customer File

- Detailed Description of the Rule:
  This rule is responsible for opening the CUSTFILE, which is an indexed VSAM file containing customer records.
- What it proposes to do:
  It ensures that the customer file is successfully opened for input before any reading operations are performed.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Set APPL-RESULT to 8 (initial error state)
2. Execute OPEN INPUT operation on CUSTFILE-FILE
3. Check CUSTFILE-STATUS:
   - If CUSTFILE-STATUS is '00' (successful open):
     - Set APPL-RESULT to 0 (success)
   - Else:
     - Set APPL-RESULT to 12 (error state)
4. If APPL-RESULT is not 0:
   - Display 'ERROR OPENING CUSTFILE' message
   - Move CUSTFILE-STATUS to IO-STATUS
   - Call Z-DISPLAY-IO-STATUS subroutine to display detailed file status:
     - If IO-STATUS is not numeric or IO-STAT1 is '9':
       - Move IO-STAT1 to IO-STATUS-04(1:1)
       - Move 0 to TWO-BYTES-BINARY
       - Move IO-STAT2 to TWO-BYTES-RIGHT
       - Move TWO-BYTES-BINARY to IO-STATUS-0403
       - Display 'FILE STATUS IS: NNNN' followed by IO-STATUS-04
     - Else:
       - Move '0000' to IO-STATUS-04
       - Move IO-STATUS to IO-STATUS-04(3:2)
       - Display 'FILE STATUS IS: NNNN' followed by IO-STATUS-04
   - Call Z-ABEND-PROGRAM subroutine to terminate the program abnormally:
     - Display 'ABENDING PROGRAM'
     - Set TIMING to 0
     - Set ABCODE to 999
     - Call 'CEE3ABD' to abort the program
5. If APPL-RESULT is 0, continue with program execution
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 2
```markdown
- ## Rule / Function / Method - Read Customer Record

- Detailed Description of the Rule:
  This rule reads the next customer record from the CUSTFILE sequentially.
- What it proposes to do:
  It retrieves customer data from the file and handles end-of-file and error conditions.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Execute READ operation on CUSTFILE-FILE, reading into CUSTOMER-RECORD
2. Check CUSTFILE-STATUS:
   - If CUSTFILE-STATUS is '00' (successful read):
     - Set APPL-RESULT to 0 (success)
     - Display CUSTOMER-RECORD
   - Else if CUSTFILE-STATUS is '10' (end of file):
     - Set APPL-RESULT to 16 (EOF indicator)
   - Else:
     - Set APPL-RESULT to 12 (error state)
3. If APPL-RESULT is 0 (APPL-AOK):
   - Continue to next iteration of read loop
4. Else if APPL-RESULT is 16 (APPL-EOF):
   - Set END-OF-FILE to 'Y'
5. Else:
   - Display 'ERROR READING CUSTOMER FILE' message
   - Move CUSTFILE-STATUS to IO-STATUS
   - Call Z-DISPLAY-IO-STATUS subroutine to display detailed file status
     - If IO-STATUS is not numeric or IO-STAT1 is '9':
       - Format and display file status as 'FILE STATUS IS: NNNN' where NNNN is IO-STATUS-04
     - Else:
       - Format and display file status as 'FILE STATUS IS: NNNN' where NNNN is '00' + IO-STATUS
   - Call Z-ABEND-PROGRAM subroutine to terminate the program abnormally
     - Display 'ABENDING PROGRAM'
     - Set TIMING to 0
     - Set ABCODE to 999
     - Call 'CEE3ABD' to abort the program

Note: This rule is part of a larger program that opens the CUSTFILE, reads records in a loop until end-of-file is reached, and then closes the file. The program also includes error handling for file opening and closing operations.
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 3
```markdown
- ## Rule / Function / Method - Close Customer File

- Detailed Description of the Rule:
  This rule is responsible for closing the CUSTFILE after all records have been processed.
- What it proposes to do:
  It ensures that the customer file is properly closed, maintaining data integrity.

## Rule Status: 
- Relevant for Modernization

## Algorithm
1. Set APPL-RESULT to 8 (initial error state)
2. Execute CLOSE operation on CUSTFILE-FILE
3. Check CUSTFILE-STATUS:
   - If CUSTFILE-STATUS is '00' (successful close):
     - Set APPL-RESULT to 0 (success)
   - Else:
     - Set APPL-RESULT to 12 (error state)
4. If APPL-RESULT is not 0:
   - Display 'ERROR CLOSING CUSTOMER FILE' message
   - Move CUSTFILE-STATUS to IO-STATUS
   - Call Z-DISPLAY-IO-STATUS subroutine to display detailed file status
     - If IO-STATUS is not numeric or IO-STAT1 is '9':
       - Display 'FILE STATUS IS: NNNN' followed by a formatted status code
     - Else:
       - Display 'FILE STATUS IS: NNNN' followed by '00' + the last two digits of IO-STATUS
   - Call Z-ABEND-PROGRAM subroutine to terminate the program abnormally
     - Display 'ABENDING PROGRAM'
     - Set TIMING to 0
     - Set ABCODE to 999
     - Call 'CEE3ABD' to abort the program
5. If APPL-RESULT is 0, continue with program termination
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 4
```markdown
- ## Rule / Function / Method - Display IO Status

- Detailed Description of the Rule:
  This subroutine formats and displays the file status for error reporting.
- What it proposes to do:
  It provides detailed information about file operation errors for debugging purposes.

## Rule Status: 
- Relevant for Modernization (Consider replacing with modern logging mechanisms)

## Algorithm
1. Check if IO-STATUS is not numeric OR if IO-STAT1 is '9':
   - Move IO-STAT1 to IO-STATUS-04(1:1)
   - Set TWO-BYTES-BINARY to 0
   - Move IO-STAT2 to TWO-BYTES-RIGHT
   - Move TWO-BYTES-BINARY to IO-STATUS-0403
   - Display 'FILE STATUS IS: NNNN' followed by IO-STATUS-04
2. Else:
   - Set IO-STATUS-04 to '0000'
   - Move IO-STATUS to IO-STATUS-04(3:2)
   - Display 'FILE STATUS IS: NNNN' followed by IO-STATUS-04
3. Exit the subroutine

## Additional Details:
- The subroutine name is Z-DISPLAY-IO-STATUS
- IO-STATUS-04 is a 4-digit field where:
  - IO-STATUS-0401 is the first digit (initialized to 0)
  - IO-STATUS-0403 is the last three digits (initialized to 000)
- TWO-BYTES-BINARY is a 4-digit binary field that is redefined as TWO-BYTES-ALPHA, which consists of TWO-BYTES-LEFT and TWO-BYTES-RIGHT
- The 'NNNN' in the display message is a placeholder for the actual file status code
```

<!--rule-end-->

<!--rule-start-->
## Reviewed Rule 5
```markdown
- ## Rule / Function / Method - Abend Program

- Detailed Description of the Rule:
  This subroutine handles abnormal termination of the program when unrecoverable errors occur during file operations (reading, opening, or closing).
- What it proposes to do:
  It ensures that the program terminates in a controlled manner, displaying error messages and file status information before initiating the abnormal termination.

## Rule Status: 
- Relevant for Modernization (Consider replacing with modern error handling and logging mechanisms)

## Algorithm
1. Display 'ABENDING PROGRAM' message
2. Set TIMING to 0
3. Set ABCODE to 999
4. Call 'CEE3ABD' to initiate abnormal termination

## Additional Details:
1. This subroutine is called in three scenarios:
   - Error reading customer file (in 1000-CUSTFILE-GET-NEXT)
   - Error opening customer file (in 0000-CUSTFILE-OPEN)
   - Error closing customer file (in 9000-CUSTFILE-CLOSE)
2. Before calling Z-ABEND-PROGRAM, the program:
   - Displays a specific error message for each scenario
   - Moves CUSTFILE-STATUS to IO-STATUS
   - Performs Z-DISPLAY-IO-STATUS to show detailed file status information
3. Z-DISPLAY-IO-STATUS routine:
   - Checks if IO-STATUS is not numeric or if IO-STAT1 is '9'
   - If true, it formats and displays the status as 'FILE STATUS IS: NNNN' where NNNN is a combination of IO-STAT1 and the binary value of IO-STAT2
   - If false, it displays 'FILE STATUS IS: NNNN' where NNNN is '00' followed by the IO-STATUS
4. The abnormal termination is triggered for any file operation error, not just for specific error codes
```

<!--rule-end-->