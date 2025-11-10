# Card Services Account and Payment Processing API

## Overview
This API provides comprehensive card services including account management, customer management, card management, and payment processing functionality based on modernized COBOL business rules.

## Base URL
```
http://localhost:8080/api
```

## API Endpoints

### Account Management

#### Get All Accounts
- **Endpoint**: `GET /accounts`
- **Description**: Retrieve all accounts in the system
- **Response**: 200 OK with array of AccountDTO

#### Get Account by ID
- **Endpoint**: `GET /accounts/{accountId}`
- **Description**: Retrieve a specific account by its 11-digit account ID
- **Path Parameters**:
  - `accountId` (string, required): 11-digit account identifier
- **Response**: 200 OK with AccountDTO

#### Get Accounts by Status
- **Endpoint**: `GET /accounts/status/{status}`
- **Description**: Retrieve all accounts with a specific status
- **Path Parameters**:
  - `status` (string, required): Account status (Y for active, N for inactive)
- **Response**: 200 OK with array of AccountDTO

#### Create Account
- **Endpoint**: `POST /accounts`
- **Description**: Create a new account
- **Request Body**: AccountCreateDTO
- **Validations**:
  - Account ID must be 11 digits
  - Active status must be Y or N
  - Credit limit must be positive
  - Cash credit limit must be positive
  - Open date cannot be in the future
  - Expiration date must be in the future
- **Response**: 201 Created with AccountDTO

#### Update Account
- **Endpoint**: `PUT /accounts/{accountId}`
- **Description**: Update an existing account
- **Path Parameters**:
  - `accountId` (string, required): 11-digit account identifier
- **Request Body**: AccountUpdateDTO
- **Response**: 200 OK with AccountDTO

#### Delete Account
- **Endpoint**: `DELETE /accounts/{accountId}`
- **Description**: Delete an account
- **Path Parameters**:
  - `accountId` (string, required): 11-digit account identifier
- **Response**: 204 No Content

---

### Customer Management

#### Get All Customers
- **Endpoint**: `GET /customers`
- **Description**: Retrieve all customers in the system
- **Response**: 200 OK with array of CustomerDTO

#### Get Customer by ID
- **Endpoint**: `GET /customers/{customerId}`
- **Description**: Retrieve a specific customer by their 9-digit customer ID
- **Path Parameters**:
  - `customerId` (string, required): 9-digit customer identifier
- **Response**: 200 OK with CustomerDTO

#### Get Customers by Last Name
- **Endpoint**: `GET /customers/lastname/{lastName}`
- **Description**: Retrieve all customers with a specific last name
- **Path Parameters**:
  - `lastName` (string, required): Customer last name
- **Response**: 200 OK with array of CustomerDTO

#### Create Customer
- **Endpoint**: `POST /customers`
- **Description**: Create a new customer
- **Request Body**: CustomerCreateDTO
- **Validations**:
  - Customer ID must be 9 digits
  - First name and last name are required (alphabets and spaces only)
  - SSN must be 9 digits
  - State code must be 2 characters
  - ZIP code must be 5 digits
  - Phone numbers must be in format (XXX)XXX-XXXX
  - FICO score must be between 300 and 850
  - Date of birth must be in the past
- **Response**: 201 Created with CustomerDTO

#### Update Customer
- **Endpoint**: `PUT /customers/{customerId}`
- **Description**: Update an existing customer
- **Path Parameters**:
  - `customerId` (string, required): 9-digit customer identifier
- **Request Body**: CustomerUpdateDTO
- **Response**: 200 OK with CustomerDTO

#### Delete Customer
- **Endpoint**: `DELETE /customers/{customerId}`
- **Description**: Delete a customer
- **Path Parameters**:
  - `customerId` (string, required): 9-digit customer identifier
- **Response**: 204 No Content

---

### Card Management

#### Get Card by Number
- **Endpoint**: `GET /cards/{cardNumber}`
- **Description**: Retrieve a specific card by its 16-digit card number
- **Path Parameters**:
  - `cardNumber` (string, required): 16-digit card number
- **Response**: 200 OK with CardDTO

#### Get Cards by Account ID
- **Endpoint**: `GET /cards/account/{accountId}`
- **Description**: Retrieve all cards associated with a specific account
- **Path Parameters**:
  - `accountId` (string, required): 11-digit account identifier
- **Response**: 200 OK with array of CardDTO

#### Get Card by Account and Card Number
- **Endpoint**: `GET /cards/account/{accountId}/card/{cardNumber}`
- **Description**: Retrieve a specific card by account ID and card number
- **Path Parameters**:
  - `accountId` (string, required): 11-digit account identifier
  - `cardNumber` (string, required): 16-digit card number
- **Response**: 200 OK with CardDTO

#### Get Cards List (Paginated)
- **Endpoint**: `GET /cards/list`
- **Description**: Retrieve a paginated list of cards with optional filtering
- **Query Parameters**:
  - `accountId` (string, optional): Filter by account ID
  - `cardNumber` (string, optional): Filter by card number
  - `page` (integer, optional): Page number (default: 0)
  - `size` (integer, optional): Page size (default: 7)
  - `sort` (string, optional): Sort criteria
- **Response**: 200 OK with Page<CardListDTO>

#### Create Card
- **Endpoint**: `POST /cards`
- **Description**: Create a new card
- **Request Body**: CardCreateDTO
- **Validations**:
  - Card number must be 16 digits
  - Account ID must be 11 digits
  - CVV code must be 3 digits
  - Embossed name can only contain alphabets and spaces
  - Active status must be Y or N
  - Expiration date must be in the future
- **Response**: 201 Created with CardDTO

#### Update Card
- **Endpoint**: `PUT /cards/{cardNumber}`
- **Description**: Update an existing card
- **Path Parameters**:
  - `cardNumber` (string, required): 16-digit card number
- **Request Body**: CardUpdateDTO
- **Validations**:
  - Embossed name can only contain alphabets and spaces
  - Active status must be Y or N
  - Expiration date must be in the future
- **Response**: 200 OK with CardDTO

#### Delete Card
- **Endpoint**: `DELETE /cards/{cardNumber}`
- **Description**: Delete a card
- **Path Parameters**:
  - `cardNumber` (string, required): 16-digit card number
- **Response**: 204 No Content

---

### Transaction and Payment Processing

#### Get Transaction by ID
- **Endpoint**: `GET /transactions/{transactionId}`
- **Description**: Retrieve a specific transaction by its ID
- **Path Parameters**:
  - `transactionId` (string, required): 16-digit transaction identifier
- **Response**: 200 OK with TransactionDTO

#### Get Transactions by Card Number
- **Endpoint**: `GET /transactions/card/{cardNumber}`
- **Description**: Retrieve all transactions for a specific card, ordered by timestamp descending
- **Path Parameters**:
  - `cardNumber` (string, required): 16-digit card number
- **Response**: 200 OK with array of TransactionDTO

#### Process Bill Payment
- **Endpoint**: `POST /transactions/bill-payment`
- **Description**: Process a full bill payment for an account
- **Request Body**: BillPaymentDTO
  - `accountId` (string, required): 11-digit account identifier
  - `confirmPayment` (string, optional): Confirmation flag (Y/N)
- **Business Rules**:
  - Validates account exists
  - Checks if there's a balance to pay (balance > 0)
  - Generates unique transaction ID
  - Creates transaction record with type code '02' (bill payment)
  - Updates account balance to zero
  - Returns transaction details
- **Response**: 201 Created with BillPaymentDTO containing:
  - `accountId`: Account identifier
  - `currentBalance`: New balance (0.00)
  - `paymentAmount`: Amount paid
  - `transactionId`: Generated transaction ID
  - `confirmPayment`: Confirmation status

---

## Data Models

### AccountDTO
- `accountId`: string (11 digits)
- `activeStatus`: string (Y/N)
- `currentBalance`: decimal
- `creditLimit`: decimal
- `cashCreditLimit`: decimal
- `openDate`: date
- `expirationDate`: date
- `reissueDate`: date (nullable)
- `currentCycleCredit`: decimal
- `currentCycleDebit`: decimal
- `groupId`: string (nullable)
- `availableCredit`: decimal (calculated)
- `active`: boolean (derived)
- `expired`: boolean (derived)

### CustomerDTO
- `customerId`: string (9 digits)
- `firstName`: string
- `middleName`: string (nullable)
- `lastName`: string
- `fullName`: string (calculated)
- `addressLine1`: string
- `addressLine2`: string (nullable)
- `city`: string
- `stateCode`: string (2 chars)
- `zipCode`: string (5 digits)
- `countryCode`: string (3 chars)
- `phoneNumber1`: string (nullable)
- `phoneNumber2`: string (nullable)
- `governmentIssuedId`: string (nullable)
- `dateOfBirth`: date
- `eftAccountId`: string
- `primaryCardHolderIndicator`: string (Y/N)
- `ficoCreditScore`: integer (300-850)
- `primaryCardHolder`: boolean (derived)

### CardDTO
- `cardNumber`: string (16 digits)
- `accountId`: string (11 digits)
- `embossedName`: string
- `expirationDate`: date
- `activeStatus`: string (Y/N)
- `active`: boolean (derived)
- `expired`: boolean (derived)

### TransactionDTO
- `transactionId`: string (16 digits)
- `cardNumber`: string (16 digits)
- `typeCode`: string (2 chars)
- `categoryCode`: integer
- `source`: string
- `description`: string
- `amount`: decimal
- `merchantId`: string
- `merchantName`: string
- `merchantCity`: string
- `merchantZip`: string
- `originationTimestamp`: datetime
- `processingTimestamp`: datetime

---

## Error Responses

All endpoints may return the following error responses:

- **400 Bad Request**: Invalid input data or validation failure
- **404 Not Found**: Resource not found
- **500 Internal Server Error**: Server error

Error response format:
```json
{
  "timestamp": "2024-01-01T12:00:00",
  "status": 400,
  "error": "Bad Request",
  "message": "Validation failed",
  "path": "/api/accounts"
}
```

---

## Business Rules Implementation

This API implements the following key business rules from the original COBOL programs:

1. **Account Update (COACTUPC)**: Comprehensive validation for account updates including monetary amounts, dates, and status codes
2. **Account View (COACTVWC)**: Account detail retrieval with customer information integration
3. **Bill Payment (COBIL00C)**: Full bill payment processing with transaction generation and balance updates
4. **Card List (COCRDLIC)**: Paginated card listing with filtering capabilities
5. **Card Detail (COCRDSLC)**: Card detail view with account association
6. **Card Update (COCRDUPC)**: Card information updates with validation

All validation rules from the original COBOL programs have been preserved and implemented using Jakarta Bean Validation annotations.
