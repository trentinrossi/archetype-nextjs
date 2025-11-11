# Bill Payment API - OpenAPI Summary

## Overview

This document provides a comprehensive summary of all REST API endpoints for the Bill Payment system. The system implements a complete bill payment processing workflow with account management, card cross-references, and transaction tracking.

## Base URL

```
http://localhost:8080/api
```

## Business Rules Implementation

The API implements the following business rules:

- **BR001**: Account Validation - Validates that the entered account ID exists in the system
- **BR002**: Balance Check - Verifies that the account has a positive balance to pay
- **BR003**: Payment Confirmation - Requires user confirmation before processing payment
- **BR004**: Full Balance Payment - Payment processes the full current account balance
- **BR005**: Transaction ID Generation - Generates unique sequential transaction ID
- **BR006**: Bill Payment Transaction Recording - Records bill payment with specific transaction attributes
- **BR007**: Account Balance Update - Updates account balance after successful payment

---

## 1. Account Management APIs

### 1.1 Get All Accounts

**Endpoint:** `GET /api/accounts`

**Description:** Retrieve a paginated list of all accounts

**Query Parameters:**
- `page` (optional): Page number (default: 0)
- `size` (optional): Page size (default: 20)
- `sort` (optional): Sort criteria

**Response:** `200 OK`
```json
{
  "content": [
    {
      "accountId": "ACC00001234",
      "currentBalance": 1500.00,
      "hasPositiveBalance": true,
      "createdAt": "2024-01-15T10:30:00",
      "updatedAt": "2024-01-15T14:45:00"
    }
  ],
  "pageable": {...},
  "totalElements": 100,
  "totalPages": 5
}
```

**Error Responses:**
- `400 Bad Request`: Invalid request parameters
- `500 Internal Server Error`: Server error

---

### 1.2 Get Account by ID

**Endpoint:** `GET /api/accounts/{accountId}`

**Description:** BR001: Account Validation - Retrieve an account by their account ID

**Path Parameters:**
- `accountId` (required): Account identifier (11 characters)

**Response:** `200 OK`
```json
{
  "accountId": "ACC00001234",
  "currentBalance": 1500.00,
  "hasPositiveBalance": true,
  "createdAt": "2024-01-15T10:30:00",
  "updatedAt": "2024-01-15T14:45:00"
}
```

**Error Responses:**
- `400 Bad Request`: Account ID can NOT be empty...
- `404 Not Found`: Account ID NOT found...
- `500 Internal Server Error`: Server error

---

### 1.3 Get Account Balance

**Endpoint:** `GET /api/accounts/{accountId}/balance`

**Description:** BR001: Account Validation & BR002: Balance Check - Retrieve the current balance for a specific account

**Path Parameters:**
- `accountId` (required): Account identifier (11 characters)

**Response:** `200 OK`
```json
{
  "accountId": "ACC00001234",
  "currentBalance": 1500.00,
  "hasPositiveBalance": true
}
```

**Error Responses:**
- `400 Bad Request`: Account ID can NOT be empty...
- `404 Not Found`: Account ID NOT found...
- `500 Internal Server Error`: Server error

---

### 1.4 Create Account

**Endpoint:** `POST /api/accounts`

**Description:** Create a new credit card account

**Request Body:**
```json
{
  "accountId": "ACC00001234",
  "currentBalance": 1500.00
}
```

**Validation Rules:**
- `accountId`: Required, max 11 characters, cannot be empty
- `currentBalance`: Required, must be greater than 0.01

**Response:** `201 Created`
```json
{
  "accountId": "ACC00001234",
  "currentBalance": 1500.00,
  "hasPositiveBalance": true,
  "createdAt": "2024-01-15T10:30:00",
  "updatedAt": "2024-01-15T10:30:00"
}
```

**Error Responses:**
- `400 Bad Request`: Invalid request data - Account ID can NOT be empty... or You have nothing to pay...
- `500 Internal Server Error`: Server error

---

### 1.5 Process Bill Payment

**Endpoint:** `POST /api/accounts/process-payment`

**Description:** BR001-BR007: Process a full balance bill payment for an account. Validates account, checks balance, requires confirmation, processes full payment, records transaction, and updates balance.

**Request Body:**
```json
{
  "accountId": "ACC00001234",
  "cardNumber": "4111111111111111",
  "confirmPayment": "Y"
}
```

**Validation Rules:**
- `accountId`: Required, max 11 characters, cannot be empty
- `cardNumber`: Required, max 16 characters
- `confirmPayment`: Optional, must be Y/y (confirm) or N/n (cancel)

**Workflow:**
1. If `confirmPayment` is empty or not provided: Returns account information for review
2. If `confirmPayment` is "N": Cancels payment and returns cancellation message
3. If `confirmPayment` is "Y": Processes full balance payment

**Response:** `200 OK`
```json
{
  "transactionId": 123456789,
  "accountId": "ACC00001234",
  "previousBalance": 1500.00,
  "newBalance": 0.00,
  "paymentAmount": 1500.00,
  "timestamp": "2024-01-15T14:45:00",
  "message": "Payment processed successfully. Transaction ID: 123456789",
  "transactionTypeCode": "02",
  "transactionCategoryCode": 2,
  "transactionSource": "POS TERM",
  "transactionDescription": "BILL PAYMENT - ONLINE",
  "merchantId": 999999999,
  "merchantName": "BILL PAYMENT",
  "merchantCity": "N/A",
  "merchantZip": "N/A"
}
```

**Error Responses:**
- `400 Bad Request`: Invalid request data - Account ID can NOT be empty... or Invalid value. Valid values are (Y/N)...
- `404 Not Found`: Account ID NOT found...
- `422 Unprocessable Entity`: You have nothing to pay...
- `500 Internal Server Error`: Server error

**Business Rules Applied:**
- **BR001**: Validates account exists
- **BR002**: Checks for positive balance
- **BR003**: Requires payment confirmation
- **BR004**: Processes full balance payment
- **BR005**: Generates unique transaction ID
- **BR006**: Records transaction with specific attributes
- **BR007**: Updates account balance to zero

---

### 1.6 Update Account

**Endpoint:** `PUT /api/accounts/{accountId}`

**Description:** Update account details by account ID

**Path Parameters:**
- `accountId` (required): Account identifier (11 characters)

**Request Body:**
```json
{
  "currentBalance": 2000.00
}
```

**Response:** `200 OK`
```json
{
  "accountId": "ACC00001234",
  "currentBalance": 2000.00,
  "hasPositiveBalance": true,
  "createdAt": "2024-01-15T10:30:00",
  "updatedAt": "2024-01-15T15:00:00"
}
```

**Error Responses:**
- `400 Bad Request`: Invalid request data or Account ID can NOT be empty...
- `404 Not Found`: Account ID NOT found...
- `500 Internal Server Error`: Server error

---

### 1.7 Delete Account

**Endpoint:** `DELETE /api/accounts/{accountId}`

**Description:** Delete an account by account ID

**Path Parameters:**
- `accountId` (required): Account identifier (11 characters)

**Response:** `204 No Content`

**Error Responses:**
- `400 Bad Request`: Account ID can NOT be empty...
- `404 Not Found`: Account ID NOT found...
- `500 Internal Server Error`: Server error

---

## 2. Card Cross Reference Management APIs

### 2.1 Get All Card Cross References

**Endpoint:** `GET /api/card-cross-references`

**Description:** Retrieve a paginated list of all card cross-references

**Query Parameters:**
- `page` (optional): Page number (default: 0)
- `size` (optional): Page size (default: 20)
- `sort` (optional): Sort criteria

**Response:** `200 OK`
```json
{
  "content": [
    {
      "accountId": "ACC00001234",
      "cardNumber": "4111111111111111",
      "createdAt": "2024-01-15T10:30:00",
      "updatedAt": "2024-01-15T10:30:00"
    }
  ],
  "pageable": {...},
  "totalElements": 50,
  "totalPages": 3
}
```

**Error Responses:**
- `400 Bad Request`: Invalid request parameters
- `500 Internal Server Error`: Server error

---

### 2.2 Get Card Cross Reference

**Endpoint:** `GET /api/card-cross-references/{accountId}/{cardNumber}`

**Description:** Retrieve a specific card cross-reference

**Path Parameters:**
- `accountId` (required): Account identifier (11 characters)
- `cardNumber` (required): Card number (16 characters)

**Response:** `200 OK`
```json
{
  "accountId": "ACC00001234",
  "cardNumber": "4111111111111111",
  "createdAt": "2024-01-15T10:30:00",
  "updatedAt": "2024-01-15T10:30:00"
}
```

**Error Responses:**
- `404 Not Found`: Card cross-reference not found
- `500 Internal Server Error`: Server error

---

### 2.3 Get Card Cross References by Account ID

**Endpoint:** `GET /api/card-cross-references/account/{accountId}`

**Description:** Retrieve all card cross-references for a specific account

**Path Parameters:**
- `accountId` (required): Account identifier (11 characters)

**Response:** `200 OK`
```json
[
  {
    "accountId": "ACC00001234",
    "cardNumber": "4111111111111111",
    "createdAt": "2024-01-15T10:30:00",
    "updatedAt": "2024-01-15T10:30:00"
  },
  {
    "accountId": "ACC00001234",
    "cardNumber": "4222222222222222",
    "createdAt": "2024-01-15T11:00:00",
    "updatedAt": "2024-01-15T11:00:00"
  }
]
```

**Error Responses:**
- `500 Internal Server Error`: Server error

---

### 2.4 Get Card Cross References by Card Number

**Endpoint:** `GET /api/card-cross-references/card/{cardNumber}`

**Description:** Retrieve all card cross-references for a specific card number

**Path Parameters:**
- `cardNumber` (required): Card number (16 characters)

**Response:** `200 OK`
```json
[
  {
    "accountId": "ACC00001234",
    "cardNumber": "4111111111111111",
    "createdAt": "2024-01-15T10:30:00",
    "updatedAt": "2024-01-15T10:30:00"
  }
]
```

**Error Responses:**
- `500 Internal Server Error`: Server error

---

### 2.5 Create Card Cross Reference

**Endpoint:** `POST /api/card-cross-references`

**Description:** Create a new card-to-account cross-reference

**Request Body:**
```json
{
  "accountId": "ACC00001234",
  "cardNumber": "4111111111111111"
}
```

**Validation Rules:**
- `accountId`: Required, max 11 characters, cannot be empty
- `cardNumber`: Required, max 16 characters, cannot be empty

**Response:** `201 Created`
```json
{
  "accountId": "ACC00001234",
  "cardNumber": "4111111111111111",
  "createdAt": "2024-01-15T10:30:00",
  "updatedAt": "2024-01-15T10:30:00"
}
```

**Error Responses:**
- `400 Bad Request`: Invalid request data
- `404 Not Found`: Account not found
- `409 Conflict`: Card cross-reference already exists
- `500 Internal Server Error`: Server error

---

### 2.6 Delete Card Cross Reference

**Endpoint:** `DELETE /api/card-cross-references/{accountId}/{cardNumber}`

**Description:** Delete a card cross-reference by account ID and card number

**Path Parameters:**
- `accountId` (required): Account identifier (11 characters)
- `cardNumber` (required): Card number (16 characters)

**Response:** `204 No Content`

**Error Responses:**
- `404 Not Found`: Card cross-reference not found
- `500 Internal Server Error`: Server error

---

### 2.7 Delete Card Cross References by Account ID

**Endpoint:** `DELETE /api/card-cross-references/account/{accountId}`

**Description:** Delete all card cross-references associated with a specific account

**Path Parameters:**
- `accountId` (required): Account identifier (11 characters)

**Response:** `204 No Content`

**Error Responses:**
- `500 Internal Server Error`: Server error

---

### 2.8 Delete Card Cross References by Card Number

**Endpoint:** `DELETE /api/card-cross-references/card/{cardNumber}`

**Description:** Delete all card cross-references associated with a specific card number

**Path Parameters:**
- `cardNumber` (required): Card number (16 characters)

**Response:** `204 No Content`

**Error Responses:**
- `500 Internal Server Error`: Server error

---

### 2.9 Check if Card Cross Reference Exists

**Endpoint:** `GET /api/card-cross-references/exists/{accountId}/{cardNumber}`

**Description:** Check if a card cross-reference exists for account and card

**Path Parameters:**
- `accountId` (required): Account identifier (11 characters)
- `cardNumber` (required): Card number (16 characters)

**Response:** `200 OK`
```json
true
```

**Error Responses:**
- `500 Internal Server Error`: Server error

---

### 2.10 Count Card Cross References by Account

**Endpoint:** `GET /api/card-cross-references/count/account/{accountId}`

**Description:** Count the number of card cross-references for a specific account

**Path Parameters:**
- `accountId` (required): Account identifier (11 characters)

**Response:** `200 OK`
```json
3
```

**Error Responses:**
- `500 Internal Server Error`: Server error

---

### 2.11 Count Card Cross References by Card

**Endpoint:** `GET /api/card-cross-references/count/card/{cardNumber}`

**Description:** Count the number of card cross-references for a specific card number

**Path Parameters:**
- `cardNumber` (required): Card number (16 characters)

**Response:** `200 OK`
```json
1
```

**Error Responses:**
- `500 Internal Server Error`: Server error

---

## 3. Transaction Management APIs

### 3.1 Get All Transactions

**Endpoint:** `GET /api/transactions`

**Description:** Retrieve a paginated list of all transactions

**Query Parameters:**
- `page` (optional): Page number (default: 0)
- `size` (optional): Page size (default: 20)
- `sort` (optional): Sort criteria

**Response:** `200 OK`
```json
{
  "content": [
    {
      "transactionId": 123456789,
      "transactionTypeCode": "02",
      "transactionCategoryCode": 2,
      "transactionSource": "POS TERM",
      "description": "BILL PAYMENT - ONLINE",
      "amount": 1500.00,
      "cardNumber": "4111111111111111",
      "merchantId": 999999999,
      "merchantName": "BILL PAYMENT",
      "merchantCity": "N/A",
      "merchantZip": "N/A",
      "originationTimestamp": "2024-01-15T14:45:00",
      "processingTimestamp": "2024-01-15T14:45:00",
      "accountId": "ACC00001234",
      "createdAt": "2024-01-15T14:45:00",
      "updatedAt": "2024-01-15T14:45:00"
    }
  ],
  "pageable": {...},
  "totalElements": 200,
  "totalPages": 10
}
```

**Error Responses:**
- `400 Bad Request`: Invalid request parameters
- `500 Internal Server Error`: Server error

---

### 3.2 Get Transaction by ID

**Endpoint:** `GET /api/transactions/{transactionId}`

**Description:** Retrieve a transaction by its ID

**Path Parameters:**
- `transactionId` (required): Transaction identifier

**Response:** `200 OK`
```json
{
  "transactionId": 123456789,
  "transactionTypeCode": "02",
  "transactionCategoryCode": 2,
  "transactionSource": "POS TERM",
  "description": "BILL PAYMENT - ONLINE",
  "amount": 1500.00,
  "cardNumber": "4111111111111111",
  "merchantId": 999999999,
  "merchantName": "BILL PAYMENT",
  "merchantCity": "N/A",
  "merchantZip": "N/A",
  "originationTimestamp": "2024-01-15T14:45:00",
  "processingTimestamp": "2024-01-15T14:45:00",
  "accountId": "ACC00001234",
  "createdAt": "2024-01-15T14:45:00",
  "updatedAt": "2024-01-15T14:45:00"
}
```

**Error Responses:**
- `404 Not Found`: Transaction not found
- `500 Internal Server Error`: Server error

---

### 3.3 Get Transactions by Account ID

**Endpoint:** `GET /api/transactions/account/{accountId}`

**Description:** BR006: Retrieve all transactions for a specific account

**Path Parameters:**
- `accountId` (required): Account identifier (11 characters)

**Response:** `200 OK`
```json
[
  {
    "transactionId": 123456789,
    "transactionTypeCode": "02",
    "transactionCategoryCode": 2,
    "transactionSource": "POS TERM",
    "description": "BILL PAYMENT - ONLINE",
    "amount": 1500.00,
    "cardNumber": "4111111111111111",
    "merchantId": 999999999,
    "merchantName": "BILL PAYMENT",
    "merchantCity": "N/A",
    "merchantZip": "N/A",
    "originationTimestamp": "2024-01-15T14:45:00",
    "processingTimestamp": "2024-01-15T14:45:00",
    "accountId": "ACC00001234",
    "createdAt": "2024-01-15T14:45:00",
    "updatedAt": "2024-01-15T14:45:00"
  }
]
```

**Error Responses:**
- `500 Internal Server Error`: Server error

---

### 3.4 Get Transactions by Account ID (Paginated)

**Endpoint:** `GET /api/transactions/account/{accountId}/paginated`

**Description:** BR006: Retrieve transactions for a specific account with pagination

**Path Parameters:**
- `accountId` (required): Account identifier (11 characters)

**Query Parameters:**
- `page` (optional): Page number (default: 0)
- `size` (optional): Page size (default: 20)
- `sort` (optional): Sort criteria

**Response:** `200 OK`
```json
{
  "content": [...],
  "pageable": {...},
  "totalElements": 50,
  "totalPages": 3
}
```

**Error Responses:**
- `500 Internal Server Error`: Server error

---

### 3.5 Get Transactions by Card Number

**Endpoint:** `GET /api/transactions/card/{cardNumber}`

**Description:** BR006: Retrieve all transactions for a specific card number

**Path Parameters:**
- `cardNumber` (required): Card number (16 characters)

**Response:** `200 OK`
```json
[
  {
    "transactionId": 123456789,
    "transactionTypeCode": "02",
    "transactionCategoryCode": 2,
    "transactionSource": "POS TERM",
    "description": "BILL PAYMENT - ONLINE",
    "amount": 1500.00,
    "cardNumber": "4111111111111111",
    "merchantId": 999999999,
    "merchantName": "BILL PAYMENT",
    "merchantCity": "N/A",
    "merchantZip": "N/A",
    "originationTimestamp": "2024-01-15T14:45:00",
    "processingTimestamp": "2024-01-15T14:45:00",
    "accountId": "ACC00001234",
    "createdAt": "2024-01-15T14:45:00",
    "updatedAt": "2024-01-15T14:45:00"
  }
]
```

**Error Responses:**
- `500 Internal Server Error`: Server error

---

### 3.6 Get Bill Payment Transactions

**Endpoint:** `GET /api/transactions/bill-payments`

**Description:** BR006: Retrieve all bill payment transactions (type code 02, category 2)

**Response:** `200 OK`
```json
[
  {
    "transactionId": 123456789,
    "transactionTypeCode": "02",
    "transactionCategoryCode": 2,
    "transactionSource": "POS TERM",
    "description": "BILL PAYMENT - ONLINE",
    "amount": 1500.00,
    "cardNumber": "4111111111111111",
    "merchantId": 999999999,
    "merchantName": "BILL PAYMENT",
    "merchantCity": "N/A",
    "merchantZip": "N/A",
    "originationTimestamp": "2024-01-15T14:45:00",
    "processingTimestamp": "2024-01-15T14:45:00",
    "accountId": "ACC00001234",
    "createdAt": "2024-01-15T14:45:00",
    "updatedAt": "2024-01-15T14:45:00"
  }
]
```

**Error Responses:**
- `500 Internal Server Error`: Server error

---

### 3.7 Get Bill Payment Transactions by Account

**Endpoint:** `GET /api/transactions/bill-payments/account/{accountId}`

**Description:** BR006: Retrieve bill payment transactions for a specific account

**Path Parameters:**
- `accountId` (required): Account identifier (11 characters)

**Response:** `200 OK`
```json
[
  {
    "transactionId": 123456789,
    "transactionTypeCode": "02",
    "transactionCategoryCode": 2,
    "transactionSource": "POS TERM",
    "description": "BILL PAYMENT - ONLINE",
    "amount": 1500.00,
    "cardNumber": "4111111111111111",
    "merchantId": 999999999,
    "merchantName": "BILL PAYMENT",
    "merchantCity": "N/A",
    "merchantZip": "N/A",
    "originationTimestamp": "2024-01-15T14:45:00",
    "processingTimestamp": "2024-01-15T14:45:00",
    "accountId": "ACC00001234",
    "createdAt": "2024-01-15T14:45:00",
    "updatedAt": "2024-01-15T14:45:00"
  }
]
```

**Error Responses:**
- `500 Internal Server Error`: Server error

---

### 3.8 Get Transactions by Type Code

**Endpoint:** `GET /api/transactions/type/{transactionTypeCode}`

**Description:** Retrieve transactions by transaction type code

**Path Parameters:**
- `transactionTypeCode` (required): Transaction type code (2 characters)

**Response:** `200 OK`
```json
[...]
```

**Error Responses:**
- `500 Internal Server Error`: Server error

---

### 3.9 Get Transactions by Category Code

**Endpoint:** `GET /api/transactions/category/{transactionCategoryCode}`

**Description:** Retrieve transactions by transaction category code

**Path Parameters:**
- `transactionCategoryCode` (required): Transaction category code (integer)

**Response:** `200 OK`
```json
[...]
```

**Error Responses:**
- `500 Internal Server Error`: Server error

---

### 3.10 Get Transactions by Date Range

**Endpoint:** `GET /api/transactions/date-range`

**Description:** Retrieve transactions within a specific date range

**Query Parameters:**
- `startDate` (required): Start date in ISO 8601 format (e.g., 2024-01-01T00:00:00)
- `endDate` (required): End date in ISO 8601 format (e.g., 2024-01-31T23:59:59)

**Response:** `200 OK`
```json
[...]
```

**Error Responses:**
- `400 Bad Request`: Invalid date format
- `500 Internal Server Error`: Server error

---

### 3.11 Get Transactions by Amount Range

**Endpoint:** `GET /api/transactions/amount-range`

**Description:** Retrieve transactions within a specific amount range

**Query Parameters:**
- `minAmount` (required): Minimum amount (decimal)
- `maxAmount` (required): Maximum amount (decimal)

**Response:** `200 OK`
```json
[...]
```

**Error Responses:**
- `400 Bad Request`: Invalid amount format
- `500 Internal Server Error`: Server error

---

### 3.12 Get Recent Transactions

**Endpoint:** `GET /api/transactions/recent`

**Description:** Retrieve transactions from the last N days

**Query Parameters:**
- `days` (optional): Number of days (default: 30)

**Response:** `200 OK`
```json
[...]
```

**Error Responses:**
- `400 Bad Request`: Invalid days parameter
- `500 Internal Server Error`: Server error

---

### 3.13 Get Transactions by Merchant ID

**Endpoint:** `GET /api/transactions/merchant/{merchantId}`

**Description:** Retrieve all transactions for a specific merchant

**Path Parameters:**
- `merchantId` (required): Merchant identifier

**Response:** `200 OK`
```json
[...]
```

**Error Responses:**
- `500 Internal Server Error`: Server error

---

### 3.14 Get Transactions by Merchant Name

**Endpoint:** `GET /api/transactions/merchant-name/{merchantName}`

**Description:** Retrieve all transactions for a specific merchant name

**Path Parameters:**
- `merchantName` (required): Merchant name

**Response:** `200 OK`
```json
[...]
```

**Error Responses:**
- `500 Internal Server Error`: Server error

---

### 3.15 Create Transaction

**Endpoint:** `POST /api/transactions`

**Description:** BR006: Create a new transaction record

**Request Body:**
```json
{
  "transactionTypeCode": "02",
  "transactionCategoryCode": 2,
  "transactionSource": "POS TERM",
  "description": "BILL PAYMENT - ONLINE",
  "amount": 1500.00,
  "cardNumber": "4111111111111111",
  "merchantId": 999999999,
  "merchantName": "BILL PAYMENT",
  "merchantCity": "N/A",
  "merchantZip": "N/A",
  "accountId": "ACC00001234"
}
```

**Validation Rules:**
- `transactionTypeCode`: Required, max 2 characters
- `transactionCategoryCode`: Required, integer
- `transactionSource`: Required, max 10 characters
- `description`: Required, max 50 characters
- `amount`: Required, decimal
- `cardNumber`: Required, max 16 characters
- `merchantId`: Required, long integer
- `merchantName`: Required, max 50 characters
- `merchantCity`: Required, max 50 characters
- `merchantZip`: Required, max 10 characters
- `accountId`: Required, max 11 characters

**Response:** `201 Created`
```json
{
  "transactionId": 123456789,
  "transactionTypeCode": "02",
  "transactionCategoryCode": 2,
  "transactionSource": "POS TERM",
  "description": "BILL PAYMENT - ONLINE",
  "amount": 1500.00,
  "cardNumber": "4111111111111111",
  "merchantId": 999999999,
  "merchantName": "BILL PAYMENT",
  "merchantCity": "N/A",
  "merchantZip": "N/A",
  "originationTimestamp": "2024-01-15T14:45:00",
  "processingTimestamp": "2024-01-15T14:45:00",
  "accountId": "ACC00001234",
  "createdAt": "2024-01-15T14:45:00",
  "updatedAt": "2024-01-15T14:45:00"
}
```

**Error Responses:**
- `400 Bad Request`: Invalid request data
- `404 Not Found`: Account not found
- `500 Internal Server Error`: Server error

---

### 3.16 Delete Transaction

**Endpoint:** `DELETE /api/transactions/{transactionId}`

**Description:** Delete a transaction by ID

**Path Parameters:**
- `transactionId` (required): Transaction identifier

**Response:** `204 No Content`

**Error Responses:**
- `404 Not Found`: Transaction not found
- `500 Internal Server Error`: Server error

---

### 3.17 Count Transactions by Account

**Endpoint:** `GET /api/transactions/count/account/{accountId}`

**Description:** Count the number of transactions for a specific account

**Path Parameters:**
- `accountId` (required): Account identifier (11 characters)

**Response:** `200 OK`
```json
25
```

**Error Responses:**
- `500 Internal Server Error`: Server error

---

### 3.18 Sum Transaction Amounts by Account

**Endpoint:** `GET /api/transactions/sum/account/{accountId}`

**Description:** Calculate the total sum of transaction amounts for a specific account

**Path Parameters:**
- `accountId` (required): Account identifier (11 characters)

**Response:** `200 OK`
```json
15000.00
```

**Error Responses:**
- `500 Internal Server Error`: Server error

---

### 3.19 Get Next Transaction ID

**Endpoint:** `GET /api/transactions/next-id`

**Description:** BR005: Get the next available transaction ID

**Response:** `200 OK`
```json
123456790
```

**Error Responses:**
- `500 Internal Server Error`: Server error

---

## Error Handling

All endpoints follow standard HTTP status codes and return error responses in a consistent format:

### Standard Error Response Format

```json
{
  "timestamp": "2024-01-15T14:45:00",
  "status": 400,
  "error": "Bad Request",
  "message": "Account ID can NOT be empty...",
  "path": "/api/accounts/process-payment"
}
```

### Common HTTP Status Codes

- `200 OK`: Successful request
- `201 Created`: Resource created successfully
- `204 No Content`: Successful deletion
- `400 Bad Request`: Invalid request data or validation error
- `404 Not Found`: Resource not found
- `409 Conflict`: Resource already exists
- `422 Unprocessable Entity`: Business rule validation failed
- `500 Internal Server Error`: Server error

---

## Data Models

### Account
```json
{
  "accountId": "string (11 chars)",
  "currentBalance": "decimal (13,2)",
  "hasPositiveBalance": "boolean",
  "createdAt": "timestamp",
  "updatedAt": "timestamp"
}
```

### CardCrossReference
```json
{
  "accountId": "string (11 chars)",
  "cardNumber": "string (16 chars)",
  "createdAt": "timestamp",
  "updatedAt": "timestamp"
}
```

### Transaction
```json
{
  "transactionId": "long",
  "transactionTypeCode": "string (2 chars)",
  "transactionCategoryCode": "integer",
  "transactionSource": "string (10 chars)",
  "description": "string (50 chars)",
  "amount": "decimal (11,2)",
  "cardNumber": "string (16 chars)",
  "merchantId": "long",
  "merchantName": "string (50 chars)",
  "merchantCity": "string (50 chars)",
  "merchantZip": "string (10 chars)",
  "originationTimestamp": "timestamp",
  "processingTimestamp": "timestamp",
  "accountId": "string (11 chars)",
  "createdAt": "timestamp",
  "updatedAt": "timestamp"
}
```

---

## Bill Payment Workflow

The complete bill payment workflow involves the following steps:

1. **Get Account Balance** (`GET /api/accounts/{accountId}/balance`)
   - Retrieve current account balance
   - Verify account has positive balance

2. **Initiate Payment** (`POST /api/accounts/process-payment` with empty `confirmPayment`)
   - Display account information for review
   - Show payment amount (full balance)

3. **Confirm Payment** (`POST /api/accounts/process-payment` with `confirmPayment: "Y"`)
   - Validate account (BR001)
   - Check positive balance (BR002)
   - Require confirmation (BR003)
   - Process full balance payment (BR004)
   - Generate transaction ID (BR005)
   - Record transaction (BR006)
   - Update account balance to zero (BR007)

4. **View Transaction** (`GET /api/transactions/{transactionId}`)
   - Retrieve transaction details
   - Verify payment was recorded

5. **Verify Balance** (`GET /api/accounts/{accountId}/balance`)
   - Confirm balance is now zero

---

## Authentication & Authorization

**Note:** This API currently does not implement authentication or authorization. In a production environment, you should add:

- JWT or OAuth2 authentication
- Role-based access control (RBAC)
- API key management
- Rate limiting
- CORS configuration

---

## Pagination

All list endpoints support pagination with the following query parameters:

- `page`: Page number (0-indexed, default: 0)
- `size`: Number of items per page (default: 20)
- `sort`: Sort criteria (e.g., `createdAt,desc`)

Example:
```
GET /api/accounts?page=0&size=10&sort=createdAt,desc
```

---

## Testing

### Sample Test Scenarios

#### 1. Create Account and Process Payment
```bash
# Create account
POST /api/accounts
{
  "accountId": "ACC00001234",
  "currentBalance": 1500.00
}

# Check balance
GET /api/accounts/ACC00001234/balance

# Process payment
POST /api/accounts/process-payment
{
  "accountId": "ACC00001234",
  "cardNumber": "4111111111111111",
  "confirmPayment": "Y"
}

# Verify balance is zero
GET /api/accounts/ACC00001234/balance
```

#### 2. View Transaction History
```bash
# Get all transactions for account
GET /api/transactions/account/ACC00001234

# Get bill payment transactions
GET /api/transactions/bill-payments/account/ACC00001234
```

#### 3. Manage Card Cross References
```bash
# Create cross-reference
POST /api/card-cross-references
{
  "accountId": "ACC00001234",
  "cardNumber": "4111111111111111"
}

# Get all cards for account
GET /api/card-cross-references/account/ACC00001234
```

---

## Version Information

- **API Version:** 1.0.0
- **Spring Boot Version:** 3.5.5
- **Java Version:** 21
- **Database:** PostgreSQL

---

## Support & Contact

For questions or issues with the API, please contact the development team.

---

**Last Updated:** 2024-01-15
