# Card Management - Quick Start Guide

## Overview

This guide helps you get started with the Card Management feature that was just generated.

## What Was Generated

A complete, production-ready Card Management system with:
- **List View**: Paginated card list with filtering (CCRDLIA)
- **Detail View**: View card details (COCRDSLC)
- **Edit View**: Update card information (COCRDUPC)
- **Create View**: Add new cards
- **Full Validation**: All business rules enforced
- **API Integration**: Complete backend connectivity

## Quick Access URLs

Once the application is running:

- **Card List**: http://localhost:3000/cards
- **Create Card**: http://localhost:3000/cards/new
- **View Card**: http://localhost:3000/cards/[cardNumber]
- **Edit Card**: http://localhost:3000/cards/[cardNumber]/edit

## Key Features

### 1. Card List Page (`/cards`)

**Features:**
- Displays 7 cards per page (COBOL specification)
- Filter by Account ID (11 digits) or Card Number (16 digits)
- Select cards with action codes:
  - `S` = View details
  - `U` = Update card
- Navigate with keyboard shortcuts:
  - `F3` = Exit to menu
  - `F7` = Previous page
  - `F8` = Next page
  - `Enter` = Execute action

**Usage:**
1. Enter filter criteria (optional)
2. Click "Apply Filters" or press Enter
3. Type `S` or `U` in the Action column
4. Press Enter or click "Execute Action"

### 2. Card Detail Page (`/cards/[cardNumber]`)

**Features:**
- View all card information
- See active/inactive status
- Check expiration status
- Edit or delete card
- Return to list

**Usage:**
1. Click "Edit Card" to modify
2. Click "Delete Card" to remove
3. Click "Back to List" to return

### 3. Card Edit Page (`/cards/[cardNumber]/edit`)

**Features:**
- Update embossed name
- Change expiration date
- Toggle active status
- Real-time validation
- Error highlighting

**Validation Rules:**
- Embossed name: Alphabets and spaces only
- Expiration date: Must be in future
- Active status: Y or N

**Usage:**
1. Modify fields as needed
2. Click "Save Changes" to update
3. Click "Cancel" to discard changes

### 4. Card Create Page (`/cards/new`)

**Features:**
- Enter new card details
- Full validation on all fields
- Real-time error feedback
- Help text with rules

**Required Fields:**
- Card Number (16 digits)
- Account ID (11 digits)
- Embossed Name (alphabets and spaces)
- Expiration Date (future date)
- Active Status (Y or N)

**Usage:**
1. Fill in all required fields
2. Click "Create Card" to save
3. Click "Cancel" to return to list

## Validation Rules

### Account ID Filter
- Optional field
- Must be exactly 11 digits if provided
- Error: "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"

### Card Number Filter
- Optional field
- Must be exactly 16 digits if provided
- Error: "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"

### Card Number (Create)
- Required field
- Must be exactly 16 digits
- Numeric only

### Account ID (Create)
- Required field
- Must be exactly 11 digits
- Numeric only

### Embossed Name
- Required field
- Alphabets and spaces only
- No special characters or numbers

### Expiration Date
- Required field
- Must be a future date
- Cannot be today or in the past

### Active Status
- Required field
- Must be 'Y' (Active) or 'N' (Inactive)

## User Messages

The system displays these messages based on user actions:

| Code | Type | Message |
|------|------|---------|
| MSG001 | Error | ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER |
| MSG002 | Error | CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER |
| MSG003 | Error | MORE THAN ONE ACTION SELECTED |
| MSG004 | Error | INVALID ACTION CODE |
| MSG005 | Info | NO RECORDS FOUND FOR THIS SEARCH CONDITION |
| MSG006 | Info | NO PREVIOUS PAGES TO DISPLAY |
| MSG007 | Info | NO MORE PAGES TO DISPLAY |
| MSG008 | Info | NO MORE RECORDS TO SHOW |

## Keyboard Shortcuts

| Key | Action | Available On |
|-----|--------|--------------|
| F3 | Exit to menu | List page |
| F7 | Previous page | List page |
| F8 | Next page | List page |
| Enter | Execute action | List page |

## API Endpoints Used

The frontend communicates with these backend endpoints:

| Method | Endpoint | Purpose |
|--------|----------|---------|
| GET | `/api/cards?page=0&size=7` | Get paginated card list |
| GET | `/api/cards?accountId=12345678901` | Filter by account |
| GET | `/api/cards?cardNumber=1234567890123456` | Filter by card |
| GET | `/api/cards/{cardNumber}` | Get card details |
| POST | `/api/cards` | Create new card |
| PUT | `/api/cards/{cardNumber}` | Update card |
| DELETE | `/api/cards/{cardNumber}` | Delete card |

## Common Workflows

### Workflow 1: View Card Details
1. Navigate to `/cards`
2. Find the card in the list
3. Enter `S` in the Action column
4. Press Enter or click "Execute Action"
5. View card details
6. Click "Back to List" to return

### Workflow 2: Update Card
1. Navigate to `/cards`
2. Find the card in the list
3. Enter `U` in the Action column
4. Press Enter or click "Execute Action"
5. Modify fields as needed
6. Click "Save Changes"
7. Redirected to detail view

### Workflow 3: Create New Card
1. Navigate to `/cards`
2. Click "Create Card" button (top right)
3. Fill in all required fields
4. Click "Create Card"
5. Redirected to card list

### Workflow 4: Filter Cards
1. Navigate to `/cards`
2. Enter Account ID or Card Number in filters
3. Click "Apply Filters"
4. View filtered results
5. Click "Clear Filters" to reset

### Workflow 5: Navigate Pages
1. Navigate to `/cards`
2. Use "Next (F8)" button or press F8
3. Use "Previous (F7)" button or press F7
4. Page number updates in header

## Troubleshooting

### Issue: "Failed to fetch cards"
**Solution**: Check that the backend API is running and accessible

### Issue: "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"
**Solution**: Ensure Account ID is exactly 11 digits (no spaces or letters)

### Issue: "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"
**Solution**: Ensure Card Number is exactly 16 digits (no spaces or letters)

### Issue: "MORE THAN ONE ACTION SELECTED"
**Solution**: Only select one card at a time (clear other selections)

### Issue: "INVALID ACTION CODE"
**Solution**: Only use 'S' (view) or 'U' (update) in Action column

### Issue: "Expiration date must be in the future"
**Solution**: Select a date that is after today

### Issue: "Embossed name can only contain alphabets and spaces"
**Solution**: Remove numbers and special characters from name

## Testing Checklist

Use this checklist to verify the implementation:

### List Page
- [ ] Page loads without errors
- [ ] Displays up to 7 cards per page
- [ ] Filter by Account ID works
- [ ] Filter by Card Number works
- [ ] Clear filters works
- [ ] Selection with 'S' navigates to detail
- [ ] Selection with 'U' navigates to edit
- [ ] Multiple selection shows error
- [ ] Invalid action code shows error
- [ ] Next page button works
- [ ] Previous page button works
- [ ] F3 exits to menu
- [ ] F7 goes to previous page
- [ ] F8 goes to next page
- [ ] Date/time updates in header
- [ ] Page number displays correctly
- [ ] Empty state shows message

### Detail Page
- [ ] Card details display correctly
- [ ] Edit button navigates to edit page
- [ ] Delete button prompts confirmation
- [ ] Delete removes card and returns to list
- [ ] Back button returns to list
- [ ] Active/Inactive status displays correctly
- [ ] Expiration warning shows if expired

### Edit Page
- [ ] Form loads with current values
- [ ] Card number is read-only
- [ ] Account ID is read-only
- [ ] Embossed name validation works
- [ ] Expiration date validation works
- [ ] Active status dropdown works
- [ ] Save updates card and redirects
- [ ] Cancel returns without saving
- [ ] Validation errors display correctly

### Create Page
- [ ] All fields are editable
- [ ] Card number validation works (16 digits)
- [ ] Account ID validation works (11 digits)
- [ ] Embossed name validation works
- [ ] Expiration date validation works
- [ ] Active status dropdown works
- [ ] Create saves card and redirects
- [ ] Cancel returns without saving
- [ ] Validation errors display correctly

## Next Steps

1. **Add to Navigation**: Add a link to `/cards` in your main menu
2. **Test with Real Data**: Connect to backend and test with actual data
3. **Customize Styling**: Adjust colors/spacing if needed (optional)
4. **Add Permissions**: Implement role-based access control if needed
5. **Add Analytics**: Track user interactions if needed

## Support

For issues or questions:
1. Check the validation rules above
2. Review the error messages
3. Check browser console for errors
4. Verify backend API is running
5. Review the CODEGEN_SUMMARY.md for technical details

## Summary

You now have a complete, production-ready Card Management system with:
- ✅ Full CRUD operations
- ✅ Pagination (7 records per page)
- ✅ Filtering by Account ID and Card Number
- ✅ Real-time validation
- ✅ Error handling
- ✅ Keyboard shortcuts
- ✅ User-friendly messages
- ✅ Responsive design

Start by navigating to http://localhost:3000/cards and explore the features!
