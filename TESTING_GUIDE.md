# Testing Guide - Card Transaction Lifecycle Management

This guide provides comprehensive testing procedures for the Card Transaction Lifecycle Management application.

---

## Test Environment Setup

### Prerequisites
1. Backend API running at `http://localhost:8080/api`
2. Frontend application running at `http://localhost:3000`
3. Test data available in the backend database
4. Modern web browser (Chrome, Firefox, Safari, or Edge)

---

## Functional Testing

### 1. Dashboard Testing

#### Test Case 1.1: Dashboard Load
**Steps**:
1. Navigate to `http://localhost:3000`
2. Verify dashboard loads successfully

**Expected Results**:
- ✅ Dashboard displays without errors
- ✅ Statistics cards show correct counts
- ✅ Navigation cards are visible
- ✅ Quick action buttons are present

#### Test Case 1.2: Dashboard Statistics
**Steps**:
1. Note the transaction count on dashboard
2. Navigate to Transactions page
3. Verify count matches

**Expected Results**:
- ✅ Statistics are accurate
- ✅ Numbers update when data changes

#### Test Case 1.3: Navigation Cards
**Steps**:
1. Click each navigation card
2. Verify correct page loads

**Expected Results**:
- ✅ Transactions card → Transactions page
- ✅ Accounts card → Accounts page
- ✅ Cards card → Cards page
- ✅ Cross References card → Cross References page

---

### 2. Transaction Management Testing

#### Test Case 2.1: View Transactions List
**Steps**:
1. Click "Transactions" navigation card
2. Verify transaction list displays

**Expected Results**:
- ✅ Transactions table displays
- ✅ Pagination controls visible
- ✅ Filter options available
- ✅ Card numbers are masked (****XXXX)
- ✅ Amounts formatted as currency
- ✅ Dates formatted correctly

#### Test Case 2.2: Filter by Card Number
**Steps**:
1. Enter a valid card number in filter
2. Click "Apply Filters"

**Expected Results**:
- ✅ Only transactions for that card display
- ✅ Page resets to 1
- ✅ Results update correctly

#### Test Case 2.3: Filter by Date Range
**Steps**:
1. Select start date
2. Select end date
3. Click "Apply Filters"

**Expected Results**:
- ✅ Only transactions in date range display
- ✅ Dates are validated
- ✅ Results update correctly

#### Test Case 2.4: Create Transaction - Valid Data
**Steps**:
1. Click "Create Transaction" button
2. Fill all required fields with valid data:
   - Card Number: 16 digits
   - Type Code: 2 digits
   - Category Code: 4 digits
   - Source: max 10 chars
   - Amount: valid decimal
   - Merchant ID: 9 digits
   - Merchant Name: max 50 chars
   - Timestamps: valid dates
3. Click "Create Transaction"

**Expected Results**:
- ✅ Success message displays
- ✅ Redirects to transactions list
- ✅ New transaction appears in list

#### Test Case 2.5: Create Transaction - Invalid Card (Error 100)
**Steps**:
1. Click "Create Transaction"
2. Enter invalid/non-existent card number
3. Fill other required fields
4. Click "Create Transaction"

**Expected Results**:
- ✅ Error message: "Invalid card number. Please verify the card number is correct."
- ✅ Form remains on screen
- ✅ User can correct and retry

#### Test Case 2.6: Create Transaction - Account Not Found (Error 101)
**Steps**:
1. Enter valid card number not linked to account
2. Fill other fields
3. Submit

**Expected Results**:
- ✅ Error message: "Account not found. The card is not associated with any account."

#### Test Case 2.7: Create Transaction - Over Limit (Error 102)
**Steps**:
1. Enter amount exceeding account credit limit
2. Submit transaction

**Expected Results**:
- ✅ Error message: "Transaction exceeds account limit. Please contact support."

#### Test Case 2.8: Create Transaction - Expired Account (Error 103)
**Steps**:
1. Use card linked to expired account
2. Submit transaction

**Expected Results**:
- ✅ Error message: "Account has expired. Please contact support."

#### Test Case 2.9: View Transaction Details
**Steps**:
1. Click "View" on any transaction
2. Verify details page loads

**Expected Results**:
- ✅ All transaction fields display correctly
- ✅ Merchant information section visible
- ✅ Timestamp information section visible
- ✅ Back button works
- ✅ Delete button present

#### Test Case 2.10: Delete Transaction
**Steps**:
1. View transaction details
2. Click "Delete Transaction"
3. Confirm deletion

**Expected Results**:
- ✅ Confirmation dialog appears
- ✅ Transaction deleted successfully
- ✅ Redirects to transactions list
- ✅ Transaction no longer in list

#### Test Case 2.11: Pagination
**Steps**:
1. Navigate through pages using pagination controls
2. Test Previous/Next buttons
3. Verify page numbers

**Expected Results**:
- ✅ Correct transactions display per page
- ✅ Page numbers accurate
- ✅ Previous disabled on first page
- ✅ Next disabled on last page

---

### 3. Account Management Testing

#### Test Case 3.1: View Accounts List
**Steps**:
1. Navigate to Accounts page
2. Verify account list displays

**Expected Results**:
- ✅ Accounts table displays
- ✅ All columns visible
- ✅ Currency formatted correctly
- ✅ Dates formatted correctly

#### Test Case 3.2: Create Account - Valid Data
**Steps**:
1. Click "Create Account"
2. Fill all required fields:
   - Account ID: numeric, max 11 digits
   - Credit Limit: positive number
   - Current Balance: number
   - Cycle Credit: number
   - Cycle Debit: number
   - Expiration Date: future date
3. Submit form

**Expected Results**:
- ✅ Success message displays
- ✅ Redirects to accounts list
- ✅ New account appears in list

#### Test Case 3.3: Create Account - Invalid Data
**Steps**:
1. Try to submit with:
   - Non-numeric Account ID
   - Negative credit limit
   - Invalid date format
2. Verify validation errors

**Expected Results**:
- ✅ Field-specific error messages display
- ✅ Form does not submit
- ✅ User can correct errors

#### Test Case 3.4: View Account Details
**Steps**:
1. Click "View" on any account
2. Verify details page loads

**Expected Results**:
- ✅ All account information displays
- ✅ Statistics cards show correct values
- ✅ Available credit calculated correctly
- ✅ Credit utilization percentage correct
- ✅ Cycle information displays

#### Test Case 3.5: Delete Account
**Steps**:
1. View account details
2. Click "Delete Account"
3. Confirm deletion

**Expected Results**:
- ✅ Confirmation dialog appears
- ✅ Account deleted successfully
- ✅ Redirects to accounts list
- ✅ Account no longer in list

---

### 4. Card Management Testing

#### Test Case 4.1: View Cards List
**Steps**:
1. Navigate to Cards page
2. Verify card list displays

**Expected Results**:
- ✅ Cards table displays
- ✅ Card numbers masked
- ✅ Status displays correctly
- ✅ Pagination works

#### Test Case 4.2: Create Card - Valid Data
**Steps**:
1. Click "Create Card"
2. Enter:
   - Card Number: 16 digits
   - Status: ACTIVE
   - Card Details: optional text
3. Submit

**Expected Results**:
- ✅ Success message displays
- ✅ Redirects to cards list
- ✅ New card appears in list

#### Test Case 4.3: Create Card - Invalid Card Number
**Steps**:
1. Enter card number with less than 16 digits
2. Try to submit

**Expected Results**:
- ✅ Validation error displays
- ✅ Form does not submit

#### Test Case 4.4: View Card Details
**Steps**:
1. Click "View" on any card
2. Verify details page loads

**Expected Results**:
- ✅ Card information displays
- ✅ Status badge shows correct color
- ✅ Timestamps display correctly

#### Test Case 4.5: Delete Card
**Steps**:
1. View card details
2. Click "Delete Card"
3. Confirm deletion

**Expected Results**:
- ✅ Confirmation dialog appears
- ✅ Card deleted successfully
- ✅ Redirects to cards list

---

### 5. Card Cross Reference Testing

#### Test Case 5.1: Search by Card Number
**Steps**:
1. Navigate to Cross References page
2. Select "Card Number" search type
3. Enter valid card number
4. Click "Search"

**Expected Results**:
- ✅ Cross reference displays
- ✅ Account ID shown
- ✅ Customer ID shown
- ✅ Created date shown

#### Test Case 5.2: Search by Account ID
**Steps**:
1. Select "Account ID" search type
2. Enter valid account ID
3. Click "Search"

**Expected Results**:
- ✅ All cards for account display
- ✅ Multiple results if applicable

#### Test Case 5.3: Create Cross Reference - Valid Data
**Steps**:
1. Click "Create Cross Reference"
2. Enter:
   - Card Number: 16 digits
   - Account ID: numeric
   - Customer ID: numeric
3. Submit

**Expected Results**:
- ✅ Success message displays
- ✅ Redirects to cross references page
- ✅ Can search and find new reference

#### Test Case 5.4: Create Cross Reference - Invalid Data
**Steps**:
1. Try to submit with:
   - Invalid card number format
   - Non-numeric account ID
   - Non-numeric customer ID

**Expected Results**:
- ✅ Validation errors display
- ✅ Form does not submit

#### Test Case 5.5: Delete Cross Reference
**Steps**:
1. Search for a cross reference
2. Click "Delete" button
3. Confirm deletion

**Expected Results**:
- ✅ Confirmation dialog appears
- ✅ Cross reference deleted
- ✅ Results refresh

---

## UI/UX Testing

### Test Case 6.1: Responsive Design
**Steps**:
1. Test on different screen sizes:
   - Desktop (1920x1080)
   - Tablet (768x1024)
   - Mobile (375x667)

**Expected Results**:
- ✅ Layout adapts to screen size
- ✅ All elements accessible
- ✅ No horizontal scrolling
- ✅ Touch targets adequate on mobile

### Test Case 6.2: Loading States
**Steps**:
1. Observe loading indicators during:
   - Page loads
   - Form submissions
   - Data fetching

**Expected Results**:
- ✅ Loading spinners display
- ✅ Buttons disabled during loading
- ✅ Loading text appears

### Test Case 6.3: Error States
**Steps**:
1. Disconnect from API
2. Try to load data

**Expected Results**:
- ✅ Error messages display
- ✅ Retry buttons available
- ✅ User can recover

### Test Case 6.4: Empty States
**Steps**:
1. View pages with no data

**Expected Results**:
- ✅ Helpful empty state messages
- ✅ Call-to-action buttons present
- ✅ Icons/illustrations display

---

## Performance Testing

### Test Case 7.1: Page Load Time
**Steps**:
1. Clear browser cache
2. Load each page
3. Measure load time

**Expected Results**:
- ✅ Dashboard loads < 2 seconds
- ✅ List pages load < 3 seconds
- ✅ Detail pages load < 2 seconds

### Test Case 7.2: Large Dataset Handling
**Steps**:
1. Load page with 100+ records
2. Test pagination
3. Test filtering

**Expected Results**:
- ✅ Pagination handles large datasets
- ✅ Filtering remains responsive
- ✅ No browser freezing

---

## Browser Compatibility Testing

### Test Case 8.1: Cross-Browser Testing
**Browsers to Test**:
- Chrome (latest)
- Firefox (latest)
- Safari (latest)
- Edge (latest)

**Expected Results**:
- ✅ All features work in all browsers
- ✅ Styling consistent
- ✅ No console errors

---

## Security Testing

### Test Case 9.1: Authentication
**Steps**:
1. Verify API calls include auth headers
2. Test with invalid token

**Expected Results**:
- ✅ Auth headers present
- ✅ Invalid token rejected

### Test Case 9.2: Data Validation
**Steps**:
1. Try to submit malicious data
2. Test XSS attempts

**Expected Results**:
- ✅ Input sanitized
- ✅ XSS prevented

---

## Regression Testing Checklist

After any code changes, verify:
- [ ] All CRUD operations still work
- [ ] Pagination functions correctly
- [ ] Filtering works as expected
- [ ] Form validation works
- [ ] Error handling works
- [ ] Navigation works
- [ ] No console errors
- [ ] No broken links
- [ ] Styling intact

---

## Bug Reporting Template

When reporting bugs, include:

```
**Title**: Brief description

**Environment**:
- Browser: 
- OS: 
- Screen Size: 

**Steps to Reproduce**:
1. 
2. 
3. 

**Expected Result**:

**Actual Result**:

**Screenshots**: (if applicable)

**Console Errors**: (if any)

**Additional Notes**:
```

---

## Test Data Requirements

### Sample Transaction Data
```json
{
  "cardNumber": "1234567890123456",
  "typeCode": "01",
  "categoryCode": "5411",
  "source": "POS",
  "description": "Test transaction",
  "amount": 100.50,
  "merchantId": 123456789,
  "merchantName": "Test Merchant",
  "merchantCity": "New York",
  "merchantZip": "10001",
  "originalTimestamp": "2024-01-15T10:30:00",
  "processingTimestamp": "2024-01-15T10:30:05"
}
```

### Sample Account Data
```json
{
  "accountId": 12345678901,
  "currentBalance": 1000.00,
  "creditLimit": 5000.00,
  "currentCycleCredit": 500.00,
  "currentCycleDebit": 200.00,
  "expirationDate": "2025-12-31"
}
```

---

## Automated Testing (Future Enhancement)

Consider implementing:
- Unit tests with Jest
- Integration tests with React Testing Library
- E2E tests with Cypress or Playwright
- API tests with Supertest

---

**Last Updated**: 2024
**Version**: 1.0.0
