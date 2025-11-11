# API Documentation Summary

## Application: CBACT01C - Account Data File Reader and Printer

### Overview
This document provides a comprehensive summary of all REST API endpoints generated for the Account and Card Data Management system. The application implements a Spring Boot REST API that manages customer account information with complete CRUD operations and business rule implementations.

---

## Base URL
```
http://localhost:8080/api
```

---

## API Endpoints

### 1. Account Management APIs

#### 1.1 Get All Accounts (Paginated)
**Endpoint:** `GET /api/accounts`

**Description:** Retrieve a paginated list of all accounts from the VSAM KSDS file.

**Query Parameters:**
- `page` (optional, default: 0) - Page number
- `size` (optional, default: 20) - Number of records per page
- `sort` (optional) - Sort criteria (e.g., `acctId,asc`)

**Response:** `200 OK`
```json
{
  "content": [
    {
      "acctId": "12345678901",
      "acctActiveStatus": "A",
      "acctCurrBal": 1500.50,
      "acctCreditLimit": 5000.00,
      "acctCashCreditLimit": 1000.00,
      "acctOpenDate": "2023-01-15",
      "acctExpirationDate": "2026-01-15",
      "acctReissueDate": "2024-01-15",
      "acctCurrCycCredit": 250.00,
      "acctCurrCycDebit": 150.00,
      "acctGroupId": "GRP001",
      "availableCredit": 3500.50,
      "availableCashCredit": -500.50,
      "currentCycleNetAmount": 100.00,
      "isActive": true,
      "isExpired": false,
      "hasBeenReissued": true,
      "activeStatusDisplayName": "Active",
      "createdAt": "2023-01-15T10:30:00",
      "updatedAt": "2024-01-15T14:45:00"
    }
  ],
  "pageable": {
    "pageNumber": 0,
    "pageSize": 20
  },
  "totalElements": 100,
  "totalPages": 5
}
```

**Error Responses:**
- `400 Bad Request` - Invalid request parameters
- `500 Internal Server Error` - Server error

**Business Rules Implemented:**
- None specific (standard pagination)

---

#### 1.2 Get Account by ID
**Endpoint:** `GET /api/accounts/{id}`

**Description:** Retrieve a single account by its unique 11-digit account identifier.

**Path Parameters:**
- `id` (required) - 11-digit account identifier (e.g., "12345678901")

**Response:** `200 OK`
```json
{
  "acctId": "12345678901",
  "acctActiveStatus": "A",
  "acctCurrBal": 1500.50,
  "acctCreditLimit": 5000.00,
  "acctCashCreditLimit": 1000.00,
  "acctOpenDate": "2023-01-15",
  "acctExpirationDate": "2026-01-15",
  "acctReissueDate": "2024-01-15",
  "acctCurrCycCredit": 250.00,
  "acctCurrCycDebit": 150.00,
  "acctGroupId": "GRP001",
  "availableCredit": 3500.50,
  "availableCashCredit": -500.50,
  "currentCycleNetAmount": 100.00,
  "isActive": true,
  "isExpired": false,
  "hasBeenReissued": true,
  "activeStatusDisplayName": "Active",
  "createdAt": "2023-01-15T10:30:00",
  "updatedAt": "2024-01-15T14:45:00"
}
```

**Error Responses:**
- `400 Bad Request` - Invalid Account ID format (must be 11 digits)
- `404 Not Found` - Account not found
- `500 Internal Server Error` - Server error

**Validation Rules:**
- Account ID must be exactly 11 numeric digits
- Pattern: `^\d{11}$`

---

#### 1.3 Create New Account
**Endpoint:** `POST /api/accounts`

**Description:** Create a new account record in the VSAM KSDS file.

**Request Body:**
```json
{
  "acctId": "12345678901",
  "acctActiveStatus": "A",
  "acctCurrBal": 1500.50,
  "acctCreditLimit": 5000.00,
  "acctCashCreditLimit": 1000.00,
  "acctOpenDate": "2023-01-15",
  "acctExpirationDate": "2026-01-15",
  "acctReissueDate": "2024-01-15",
  "acctCurrCycCredit": 250.00,
  "acctCurrCycDebit": 150.00,
  "acctGroupId": "GRP001"
}
```

**Required Fields:**
- `acctId` - 11-digit numeric string
- `acctActiveStatus` - Single character: 'A' (active) or 'I' (inactive)
- `acctCurrBal` - Decimal value >= 0.0
- `acctCreditLimit` - Decimal value >= 0.0
- `acctCashCreditLimit` - Decimal value >= 0.0
- `acctOpenDate` - Date in YYYY-MM-DD format
- `acctExpirationDate` - Date in YYYY-MM-DD format
- `acctCurrCycCredit` - Decimal value >= 0.0
- `acctCurrCycDebit` - Decimal value >= 0.0

**Optional Fields:**
- `acctReissueDate` - Date in YYYY-MM-DD format
- `acctGroupId` - String (max 10 characters)

**Response:** `201 Created`
```json
{
  "acctId": "12345678901",
  "acctActiveStatus": "A",
  "acctCurrBal": 1500.50,
  "acctCreditLimit": 5000.00,
  "acctCashCreditLimit": 1000.00,
  "acctOpenDate": "2023-01-15",
  "acctExpirationDate": "2026-01-15",
  "acctReissueDate": "2024-01-15",
  "acctCurrCycCredit": 250.00,
  "acctCurrCycDebit": 150.00,
  "acctGroupId": "GRP001",
  "availableCredit": 3500.50,
  "availableCashCredit": -500.50,
  "currentCycleNetAmount": 100.00,
  "isActive": true,
  "isExpired": false,
  "hasBeenReissued": true,
  "activeStatusDisplayName": "Active",
  "createdAt": "2023-01-15T10:30:00",
  "updatedAt": "2024-01-15T14:45:00"
}
```

**Error Responses:**
- `400 Bad Request` - Invalid request data or validation failure
  - Account ID already exists
  - Invalid Account ID format
  - Invalid active status value
  - Negative balance/limit values
  - Expiration date before open date
  - Reissue date before open date
- `500 Internal Server Error` - Server error

**Validation Rules:**
- Account ID must be unique and match pattern `^\d{11}$`
- Active status must be 'A' or 'I'
- All monetary values must be non-negative
- Expiration date must be after open date
- Reissue date (if provided) must be after open date

---

#### 1.4 Update Existing Account
**Endpoint:** `PUT /api/accounts/{id}`

**Description:** Update account details by account ID.

**Path Parameters:**
- `id` (required) - 11-digit account identifier

**Request Body:**
```json
{
  "acctId": "12345678901",
  "acctActiveStatus": "I",
  "acctCurrBal": 2000.00,
  "acctCreditLimit": 6000.00,
  "acctCashCreditLimit": 1500.00,
  "acctOpenDate": "2023-01-15",
  "acctExpirationDate": "2027-01-15",
  "acctReissueDate": "2024-06-15",
  "acctCurrCycCredit": 300.00,
  "acctCurrCycDebit": 200.00,
  "acctGroupId": "GRP002"
}
```

**Note:** All fields except `acctId` are optional. Only provided fields will be updated.

**Response:** `200 OK`
```json
{
  "acctId": "12345678901",
  "acctActiveStatus": "I",
  "acctCurrBal": 2000.00,
  "acctCreditLimit": 6000.00,
  "acctCashCreditLimit": 1500.00,
  "acctOpenDate": "2023-01-15",
  "acctExpirationDate": "2027-01-15",
  "acctReissueDate": "2024-06-15",
  "acctCurrCycCredit": 300.00,
  "acctCurrCycDebit": 200.00,
  "acctGroupId": "GRP002",
  "availableCredit": 4000.00,
  "availableCashCredit": -500.00,
  "currentCycleNetAmount": 100.00,
  "isActive": false,
  "isExpired": false,
  "hasBeenReissued": true,
  "activeStatusDisplayName": "Inactive",
  "createdAt": "2023-01-15T10:30:00",
  "updatedAt": "2024-06-15T16:20:00"
}
```

**Error Responses:**
- `400 Bad Request` - Invalid request data or validation failure
- `404 Not Found` - Account not found
- `500 Internal Server Error` - Server error

**Validation Rules:**
- Same validation rules as Create Account apply to updated fields

---

#### 1.5 Delete Account
**Endpoint:** `DELETE /api/accounts/{id}`

**Description:** Delete an account by its account ID.

**Path Parameters:**
- `id` (required) - 11-digit account identifier

**Response:** `204 No Content`

**Error Responses:**
- `404 Not Found` - Account not found
- `500 Internal Server Error` - Server error

---

#### 1.6 Get All Accounts Sequentially
**Endpoint:** `GET /api/accounts/sequential`

**Description:** **BR-001: Sequential Account Record Processing** - Process account records sequentially from the account file until end-of-file is reached. Reads records one by one in sequential order using account ID as the key.

**Response:** `200 OK`
```json
[
  {
    "acctId": "00000000001",
    "acctActiveStatus": "A",
    "acctCurrBal": 1500.50,
    "acctCreditLimit": 5000.00,
    "acctCashCreditLimit": 1000.00,
    "acctOpenDate": "2023-01-15",
    "acctExpirationDate": "2026-01-15",
    "acctReissueDate": null,
    "acctCurrCycCredit": 250.00,
    "acctCurrCycDebit": 150.00,
    "acctGroupId": "GRP001",
    "availableCredit": 3500.50,
    "availableCashCredit": -500.50,
    "currentCycleNetAmount": 100.00,
    "isActive": true,
    "isExpired": false,
    "hasBeenReissued": false,
    "activeStatusDisplayName": "Active",
    "createdAt": "2023-01-15T10:30:00",
    "updatedAt": "2024-01-15T14:45:00"
  },
  {
    "acctId": "00000000002",
    "acctActiveStatus": "A",
    ...
  }
]
```

**Error Responses:**
- `400 Bad Request` - Account file access error (BR-003 violation)
- `500 Internal Server Error` - Server error

**Business Rules Implemented:**
- **BR-001:** Sequential Account Record Processing - Records are processed sequentially from the account file
- **BR-003:** Account File Access Control - File must be opened for input operations before reading
- **BR-004:** End of File Detection - Processing stops when end-of-file condition is detected

**Special Behaviors:**
- Returns all accounts ordered by account ID in ascending order
- Logs each account's information as it's processed (BR-002)
- Detects and logs end-of-file condition (BR-004)
- File status codes: 00 (success), 12 (error), 16 (EOF)

---

#### 1.7 Get Active Accounts
**Endpoint:** `GET /api/accounts/active`

**Description:** Retrieve all accounts with active status ('A').

**Response:** `200 OK`
```json
[
  {
    "acctId": "12345678901",
    "acctActiveStatus": "A",
    "acctCurrBal": 1500.50,
    "acctCreditLimit": 5000.00,
    "acctCashCreditLimit": 1000.00,
    "acctOpenDate": "2023-01-15",
    "acctExpirationDate": "2026-01-15",
    "acctReissueDate": "2024-01-15",
    "acctCurrCycCredit": 250.00,
    "acctCurrCycDebit": 150.00,
    "acctGroupId": "GRP001",
    "availableCredit": 3500.50,
    "availableCashCredit": -500.50,
    "currentCycleNetAmount": 100.00,
    "isActive": true,
    "isExpired": false,
    "hasBeenReissued": true,
    "activeStatusDisplayName": "Active",
    "createdAt": "2023-01-15T10:30:00",
    "updatedAt": "2024-01-15T14:45:00"
  }
]
```

**Error Responses:**
- `400 Bad Request` - Invalid request parameters
- `500 Internal Server Error` - Server error

**Filter Criteria:**
- `acctActiveStatus = 'A'`

---

#### 1.8 Get Expired Accounts
**Endpoint:** `GET /api/accounts/expired`

**Description:** Retrieve all accounts that have passed their expiration date.

**Response:** `200 OK`
```json
[
  {
    "acctId": "12345678901",
    "acctActiveStatus": "A",
    "acctCurrBal": 1500.50,
    "acctCreditLimit": 5000.00,
    "acctCashCreditLimit": 1000.00,
    "acctOpenDate": "2020-01-15",
    "acctExpirationDate": "2023-01-15",
    "acctReissueDate": null,
    "acctCurrCycCredit": 250.00,
    "acctCurrCycDebit": 150.00,
    "acctGroupId": "GRP001",
    "availableCredit": 3500.50,
    "availableCashCredit": -500.50,
    "currentCycleNetAmount": 100.00,
    "isActive": true,
    "isExpired": true,
    "hasBeenReissued": false,
    "activeStatusDisplayName": "Active",
    "createdAt": "2020-01-15T10:30:00",
    "updatedAt": "2023-01-15T14:45:00"
  }
]
```

**Error Responses:**
- `400 Bad Request` - Invalid request parameters
- `500 Internal Server Error` - Server error

**Filter Criteria:**
- `acctExpirationDate < current_date`

---

#### 1.9 Display Account Information
**Endpoint:** `GET /api/accounts/{id}/display`

**Description:** **BR-002: Account Data Display Requirements** - Display all account information fields for a specific account record. Shows Account ID, Active Status, Current Balance, Credit Limit, Cash Credit Limit, Open Date, Expiration Date, Reissue Date, Current Cycle Credit, Current Cycle Debit, and Group ID.

**Path Parameters:**
- `id` (required) - 11-digit account identifier

**Response:** `200 OK`
```json
{
  "acctId": "12345678901",
  "acctActiveStatus": "A",
  "acctCurrBal": 1500.50,
  "acctCreditLimit": 5000.00,
  "acctCashCreditLimit": 1000.00,
  "acctOpenDate": "2023-01-15",
  "acctExpirationDate": "2026-01-15",
  "acctReissueDate": "2024-01-15",
  "acctCurrCycCredit": 250.00,
  "acctCurrCycDebit": 150.00,
  "acctGroupId": "GRP001",
  "availableCredit": 3500.50,
  "availableCashCredit": -500.50,
  "currentCycleNetAmount": 100.00,
  "isActive": true,
  "isExpired": false,
  "hasBeenReissued": true,
  "activeStatusDisplayName": "Active",
  "createdAt": "2023-01-15T10:30:00",
  "updatedAt": "2024-01-15T14:45:00"
}
```

**Error Responses:**
- `400 Bad Request` - Invalid Account ID format
- `404 Not Found` - Account not found
- `500 Internal Server Error` - Server error

**Business Rules Implemented:**
- **BR-002:** Account Data Display Requirements - All account information fields are displayed

**Special Behaviors:**
- Logs all account information to the application log in a formatted display
- Returns the complete account response with all computed fields

---

#### 1.10 Process Account Sequentially
**Endpoint:** `POST /api/accounts/{id}/process`

**Description:** **BR-001 & BR-003 & BR-004:** Process a specific account record sequentially. Opens account file for input operations (BR-003), reads the account record, displays complete information (BR-002), and handles end-of-file detection (BR-004). File status must be '00' for successful operation.

**Path Parameters:**
- `id` (required) - 11-digit account identifier

**Response:** `200 OK`

**Error Responses:**
- `400 Bad Request` - Invalid Account ID format or file access error (File status not 00)
- `404 Not Found` - Account not found or end-of-file reached (File status 10)
- `500 Internal Server Error` - Server error (File status 12)

**Business Rules Implemented:**
- **BR-001:** Sequential Account Record Processing - Account is processed sequentially
- **BR-002:** Account Data Display Requirements - All account information is displayed
- **BR-003:** Account File Access Control - File is opened for input operations
- **BR-004:** End of File Detection - EOF condition is detected and handled

**File Status Codes:**
- `00` - Success (returns 200 OK)
- `10` - Record not found / EOF (returns 404 Not Found)
- `12` - File access error (returns 500 Internal Server Error)
- `16` - End of file detected

**Special Behaviors:**
- Validates account file access before processing
- Logs complete account information to application log
- Returns appropriate HTTP status based on file status code

---

#### 1.11 Check Account Exists
**Endpoint:** `GET /api/accounts/{id}/exists`

**Description:** Validate if an account with the given ID exists in the system.

**Path Parameters:**
- `id` (required) - 11-digit account identifier

**Response:** `200 OK`
```json
true
```

or

```json
false
```

**Error Responses:**
- `400 Bad Request` - Invalid Account ID format
- `500 Internal Server Error` - Server error

**Validation Rules:**
- Account ID must match pattern `^\d{11}$`

---

## Data Models

### Account Entity Fields

| Field Name | Type | Required | Description | Constraints |
|------------|------|----------|-------------|-------------|
| acctId | String | Yes | Unique 11-digit account identifier | Pattern: `^\d{11}$`, Length: 11 |
| acctActiveStatus | String | Yes | Account active status | Values: 'A' (active), 'I' (inactive), Length: 1 |
| acctCurrBal | BigDecimal | Yes | Current balance of the account | Precision: 15, Scale: 2, Min: 0.0 |
| acctCreditLimit | BigDecimal | Yes | Maximum credit limit | Precision: 15, Scale: 2, Min: 0.0 |
| acctCashCreditLimit | BigDecimal | Yes | Maximum cash credit limit | Precision: 15, Scale: 2, Min: 0.0 |
| acctOpenDate | LocalDate | Yes | Date when account was opened | Format: YYYY-MM-DD |
| acctExpirationDate | LocalDate | Yes | Date when account expires | Format: YYYY-MM-DD, Must be after open date |
| acctReissueDate | LocalDate | No | Date when account was reissued | Format: YYYY-MM-DD, Must be after open date |
| acctCurrCycCredit | BigDecimal | Yes | Current cycle credit amount | Precision: 15, Scale: 2, Min: 0.0 |
| acctCurrCycDebit | BigDecimal | Yes | Current cycle debit amount | Precision: 15, Scale: 2, Min: 0.0 |
| acctGroupId | String | No | Account group identifier | Max Length: 10 |
| createdAt | LocalDateTime | Auto | Record creation timestamp | Auto-generated |
| updatedAt | LocalDateTime | Auto | Record update timestamp | Auto-updated |

### Computed Fields (Response Only)

| Field Name | Type | Description | Calculation |
|------------|------|-------------|-------------|
| availableCredit | BigDecimal | Available credit amount | acctCreditLimit - acctCurrBal |
| availableCashCredit | BigDecimal | Available cash credit amount | acctCashCreditLimit - acctCurrBal |
| currentCycleNetAmount | BigDecimal | Net cycle amount | acctCurrCycCredit - acctCurrCycDebit |
| isActive | Boolean | Whether account is active | acctActiveStatus == 'A' |
| isExpired | Boolean | Whether account has expired | acctExpirationDate < current_date |
| hasBeenReissued | Boolean | Whether account has been reissued | acctReissueDate != null |
| activeStatusDisplayName | String | Human-readable status | "Active" or "Inactive" |

---

## Business Rules Summary

### BR-001: Sequential Account Record Processing
**Implementation:** 
- Endpoint: `GET /api/accounts/sequential`
- Endpoint: `POST /api/accounts/{id}/process`
- Account records are processed sequentially from the account file until end-of-file is reached
- Records are ordered by account ID in ascending order
- Each record is read and processed one by one

### BR-002: Account Data Display Requirements
**Implementation:**
- Endpoint: `GET /api/accounts/{id}/display`
- Endpoint: `POST /api/accounts/{id}/process`
- All account information fields are displayed for each record processed
- Includes: Account ID, Active Status, Current Balance, Credit Limit, Cash Credit Limit, Open Date, Expiration Date, Reissue Date, Current Cycle Credit, Current Cycle Debit, Group ID
- Information is logged to application log in formatted display

### BR-003: Account File Access Control
**Implementation:**
- All endpoints that access account data
- Account file must be opened for input operations before any record can be read
- File status code '00' indicates successful file access
- File status code '12' indicates file access error
- Validation occurs before processing any account operations

### BR-004: End of File Detection
**Implementation:**
- Endpoint: `GET /api/accounts/sequential`
- Processing stops when end-of-file condition is detected
- Application result set to '16' when EOF is reached
- END-OF-FILE flag set to 'Y'
- Logged to application log when detected

---

## Error Codes

| HTTP Status | Error Code | Description |
|-------------|------------|-------------|
| 400 | BAD_REQUEST | Invalid request data, validation failure, or invalid Account ID format |
| 404 | NOT_FOUND | Account not found or end-of-file reached |
| 500 | INTERNAL_SERVER_ERROR | Server error or file access error |

### File Status Codes (Business Rule Context)

| File Status | Description | HTTP Status |
|-------------|-------------|-------------|
| 00 | Success - File opened and operation completed | 200 OK |
| 10 | Record not found | 404 Not Found |
| 12 | File access error | 500 Internal Server Error |
| 16 | End of file detected | 200 OK (with EOF flag) |

---

## Validation Rules

### Account ID Validation
- Must be exactly 11 numeric digits
- Pattern: `^\d{11}$`
- Examples: "12345678901", "00000000001"
- Invalid: "123456789", "1234567890A", "123456789012"

### Active Status Validation
- Must be exactly 1 character
- Valid values: 'A' (active) or 'I' (inactive)
- Case-sensitive

### Monetary Value Validation
- All monetary fields must be non-negative (>= 0.0)
- Precision: 15 digits
- Scale: 2 decimal places
- Examples: 1500.50, 0.00, 999999999999999.99

### Date Validation
- Format: YYYY-MM-DD (ISO 8601)
- Expiration date must be after open date
- Reissue date (if provided) must be after open date
- Examples: "2023-01-15", "2026-12-31"

---

## Testing Examples

### Example 1: Create and Retrieve Account

**Step 1: Create Account**
```bash
curl -X POST http://localhost:8080/api/accounts \
  -H "Content-Type: application/json" \
  -d '{
    "acctId": "12345678901",
    "acctActiveStatus": "A",
    "acctCurrBal": 1500.50,
    "acctCreditLimit": 5000.00,
    "acctCashCreditLimit": 1000.00,
    "acctOpenDate": "2023-01-15",
    "acctExpirationDate": "2026-01-15",
    "acctCurrCycCredit": 250.00,
    "acctCurrCycDebit": 150.00,
    "acctGroupId": "GRP001"
  }'
```

**Step 2: Retrieve Account**
```bash
curl -X GET http://localhost:8080/api/accounts/12345678901
```

### Example 2: Process Account Sequentially

```bash
curl -X POST http://localhost:8080/api/accounts/12345678901/process
```

### Example 3: Get All Active Accounts

```bash
curl -X GET http://localhost:8080/api/accounts/active
```

### Example 4: Update Account Status

```bash
curl -X PUT http://localhost:8080/api/accounts/12345678901 \
  -H "Content-Type: application/json" \
  -d '{
    "acctId": "12345678901",
    "acctActiveStatus": "I"
  }'
```

### Example 5: Get Accounts Sequentially (BR-001)

```bash
curl -X GET http://localhost:8080/api/accounts/sequential
```

---

## Database Schema

### Table: accounts

```sql
CREATE TABLE accounts (
    acct_id VARCHAR(11) NOT NULL,
    acct_active_status VARCHAR(1) NOT NULL,
    acct_curr_bal DECIMAL(15, 2) NOT NULL,
    acct_credit_limit DECIMAL(15, 2) NOT NULL,
    acct_cash_credit_limit DECIMAL(15, 2) NOT NULL,
    acct_open_date DATE NOT NULL,
    acct_expiration_date DATE NOT NULL,
    acct_reissue_date DATE,
    acct_curr_cyc_credit DECIMAL(15, 2) NOT NULL,
    acct_curr_cyc_debit DECIMAL(15, 2) NOT NULL,
    acct_group_id VARCHAR(10),
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (acct_id),
    CONSTRAINT chk_acct_active_status CHECK (acct_active_status IN ('A', 'I')),
    CONSTRAINT chk_acct_id_format CHECK (acct_id ~ '^[0-9]{11}$')
);

CREATE INDEX idx_accounts_acct_group_id ON accounts(acct_group_id);
CREATE INDEX idx_accounts_acct_active_status ON accounts(acct_active_status);
CREATE INDEX idx_accounts_acct_open_date ON accounts(acct_open_date);
CREATE INDEX idx_accounts_acct_expiration_date ON accounts(acct_expiration_date);
```

---

## Technical Stack

- **Framework:** Spring Boot 3.5.5
- **Java Version:** 21
- **Database:** PostgreSQL
- **ORM:** JPA/Hibernate
- **Migration Tool:** Flyway
- **API Documentation:** OpenAPI 3.0 (Swagger)
- **Build Tool:** Maven

---

## Notes

1. All endpoints return JSON responses
2. All date fields use ISO 8601 format (YYYY-MM-DD)
3. All monetary values use BigDecimal with 2 decimal places
4. Pagination uses Spring Data's default pagination mechanism
5. All endpoints are logged for audit purposes
6. Business rules BR-001 through BR-004 are fully implemented
7. Account ID validation is enforced at all layers (DTO, Entity, Service)
8. Computed fields are calculated dynamically in the response DTO

---

## Support

For issues or questions regarding the API, please refer to the application logs or contact the development team.

**Application:** CBACT01C - Account Data File Reader and Printer  
**Version:** 1.0.0  
**Last Updated:** 2024
