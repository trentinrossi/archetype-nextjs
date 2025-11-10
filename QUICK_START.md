# Quick Start Guide - Card Management Feature

## Overview

This guide will help you quickly understand and run the Card Management feature that has been generated.

## What Was Generated

A complete, production-ready credit card management system with:
- **Card List Screen** - Browse and filter cards
- **Card Detail Screen** - View card information
- **Card Edit Screen** - Update card details
- **Card Create Screen** - Add new cards

## File Structure

```
src/
├── types/
│   └── card.ts                          # TypeScript type definitions
├── services/
│   └── cardService.ts                   # API client service
├── app/
│   ├── api/
│   │   └── cards/
│   │       ├── route.ts                 # GET /api/cards, POST /api/cards
│   │       ├── [cardNumber]/
│   │       │   └── route.ts             # GET/PUT/DELETE /api/cards/:cardNumber
│   │       └── account/
│   │           └── [accountId]/
│   │               └── route.ts         # GET /api/cards/account/:accountId
│   └── cards/
│       ├── page.tsx                     # Card list page
│       ├── [cardNumber]/
│       │   ├── page.tsx                 # Card detail page
│       │   └── edit/
│       │       └── page.tsx             # Card edit page
│       └── new/
│           └── page.tsx                 # Card create page
```

## Running the Application

### 1. Install Dependencies (if not already done)
```bash
npm install
```

### 2. Start Development Server
```bash
npm run dev
```

### 3. Access the Application
Open your browser and navigate to:
```
http://localhost:3000/cards
```

## Using the Card Management Feature

### Card List Screen (`/cards`)

**URL**: `http://localhost:3000/cards`

**Features**:
- View paginated list of cards (7 per page)
- Filter by Account ID (11 digits)
- Filter by Card Number (16 digits)
- Select cards for viewing (S) or editing (U)
- Navigate pages with F7 (previous) and F8 (next)
- Exit to main menu with F3

**How to Use**:
1. Enter filters (optional):
   - Account ID: 11-digit number
   - Card Number: 16-digit number
2. Click "Search" to apply filters
3. Enter selection code in the "Select" column:
   - `S` = View card details
   - `U` = Edit card
4. Click "Enter - Process Selection" or press Enter
5. Use pagination buttons or F7/F8 keys to navigate pages

### Card Detail Screen (`/cards/[cardNumber]`)

**URL**: `http://localhost:3000/cards/4556737586899855` (example)

**Features**:
- View complete card information
- See active/inactive status
- Check expiration status
- Edit or delete card
- Return to card list

**How to Use**:
1. Click "Edit Card" to modify card information
2. Click "Delete Card" to remove card (with confirmation)
3. Click "Back to List" to return to card list

### Card Edit Screen (`/cards/[cardNumber]/edit`)

**URL**: `http://localhost:3000/cards/4556737586899855/edit` (example)

**Features**:
- Update embossed name
- Update expiration date
- Change active status
- Real-time validation

**How to Use**:
1. Modify the editable fields:
   - Embossed Name (alphabets and spaces only)
   - Expiration Date (must be future date)
   - Active Status (Y or N)
2. Click "Save Changes" to update
3. Click "Cancel" to discard changes

**Validation Rules**:
- Embossed name: Only letters and spaces
- Expiration date: Must be in the future
- Active status: Must be Y or N

### Card Create Screen (`/cards/new`)

**URL**: `http://localhost:3000/cards/new`

**Features**:
- Create new credit card
- Full validation on all fields
- Real-time error feedback

**How to Use**:
1. Fill in all required fields:
   - Card Number (16 digits)
   - Account ID (11 digits)
   - Embossed Name (letters and spaces)
   - CVV Code (3 digits)
   - Expiration Date (future date)
   - Active Status (Y or N)
2. Click "Create Card" to add the card
3. Click "Cancel" to discard

**Validation Rules**:
- Card number: Exactly 16 digits, must be unique
- Account ID: Exactly 11 digits, must exist
- Embossed name: Only letters and spaces
- CVV code: Exactly 3 digits
- Expiration date: Must be in the future
- Active status: Must be Y or N

## Keyboard Shortcuts

The card list screen supports keyboard shortcuts for power users:

- **Enter** - Process selection
- **F3** - Exit to main menu
- **F7** - Previous page
- **F8** - Next page

## API Endpoints

The feature integrates with the following backend endpoints:

### List Cards (Paginated)
```
GET /api/cards?accountId=12345678901&cardNumber=4556737586899855&page=0&size=7
```

### Get Card by Number
```
GET /api/cards/4556737586899855
```

### Get Cards by Account
```
GET /api/cards/account/12345678901
```

### Create Card
```
POST /api/cards
Content-Type: application/json

{
  "cardNumber": "4556737586899855",
  "accountId": "12345678901",
  "embossedName": "JOHN DOE",
  "expirationDate": "2025-12-31",
  "activeStatus": "Y",
  "cvvCode": "123"
}
```

### Update Card
```
PUT /api/cards/4556737586899855
Content-Type: application/json

{
  "embossedName": "JOHN DOE",
  "expirationDate": "2025-12-31",
  "activeStatus": "Y"
}
```

### Delete Card
```
DELETE /api/cards/4556737586899855
```

## Common Issues and Solutions

### Issue: "Failed to fetch cards"
**Solution**: Ensure the backend API is running and accessible. Check the auth middleware configuration for the correct backend URL.

### Issue: Validation errors on filters
**Solution**: 
- Account ID must be exactly 11 digits
- Card Number must be exactly 16 digits
- Both are optional but must be valid if provided

### Issue: "Only one selection allowed"
**Solution**: Clear other selection codes before selecting a different card. Only one card can be selected at a time.

### Issue: "Invalid action code"
**Solution**: Use only 'S' (view) or 'U' (edit) as selection codes. Other characters are not valid.

### Issue: Card not found
**Solution**: Verify the card number is correct and the card exists in the database.

## Testing the Feature

### Test Data Requirements

To fully test the feature, you'll need:
1. At least one account in the system (11-digit account ID)
2. At least one card associated with that account
3. Valid authentication token

### Test Scenarios

#### 1. List Cards
- [ ] Load page without filters
- [ ] Filter by valid account ID
- [ ] Filter by valid card number
- [ ] Filter with invalid account ID (should show error)
- [ ] Filter with invalid card number (should show error)
- [ ] Navigate to next page
- [ ] Navigate to previous page
- [ ] Try F7 on first page (should show message)
- [ ] Try F8 on last page (should show message)

#### 2. View Card Details
- [ ] Select card with 'S' code
- [ ] Press Enter to view details
- [ ] Verify all card information displays
- [ ] Check status indicators

#### 3. Edit Card
- [ ] Select card with 'U' code
- [ ] Press Enter to edit
- [ ] Modify embossed name
- [ ] Change expiration date
- [ ] Toggle active status
- [ ] Save changes
- [ ] Verify changes in detail view

#### 4. Create Card
- [ ] Navigate to /cards/new
- [ ] Fill in all fields with valid data
- [ ] Submit form
- [ ] Verify card appears in list
- [ ] Try creating duplicate card number (should fail)

#### 5. Delete Card
- [ ] View card details
- [ ] Click delete button
- [ ] Confirm deletion
- [ ] Verify card removed from list

## Development Tips

### Adding New Features

If you need to extend this feature:

1. **Add new fields to card**:
   - Update `src/types/card.ts`
   - Update API routes if needed
   - Update service methods
   - Update UI forms

2. **Add new filters**:
   - Update `CardFilterCriteria` type
   - Update card list page state
   - Update search form
   - Update API call in service

3. **Add new actions**:
   - Add button/handler in appropriate page
   - Add service method if needed
   - Add API route if needed

### Code Style

The generated code follows these conventions:
- **Files**: camelCase for services, PascalCase for components
- **Variables**: camelCase
- **Types**: PascalCase
- **Constants**: UPPER_SNAKE_CASE
- **Functions**: camelCase with descriptive names

### Best Practices

1. **Always validate input** before API calls
2. **Handle errors gracefully** with user-friendly messages
3. **Show loading states** during async operations
4. **Use TypeScript types** for type safety
5. **Follow the archetype patterns** for consistency

## Support and Documentation

### Additional Documentation
- See `IMPLEMENTATION_SUMMARY.md` for complete technical details
- See `archetype.md` for framework patterns and conventions
- See OpenAPI summary for backend API specifications

### Architecture
- **Frontend**: Next.js 15 with React 19 and TypeScript
- **Styling**: TailwindCSS v4
- **State**: React hooks (useState, useEffect)
- **Routing**: Next.js App Router
- **API**: Next.js API routes with backend forwarding

## Next Steps

1. **Test the feature** using the test scenarios above
2. **Review the code** to understand the implementation
3. **Customize as needed** for your specific requirements
4. **Deploy to production** when ready

## Quick Reference

### URLs
- Card List: `/cards`
- Card Detail: `/cards/[cardNumber]`
- Card Edit: `/cards/[cardNumber]/edit`
- Card Create: `/cards/new`

### Selection Codes
- `S` - View card details
- `U` - Edit card

### Keyboard Shortcuts
- `Enter` - Process selection
- `F3` - Exit
- `F7` - Previous page
- `F8` - Next page

### Validation Lengths
- Card Number: 16 digits
- Account ID: 11 digits
- CVV Code: 3 digits

---

**Ready to use!** The feature is fully implemented and production-ready. Start the dev server and navigate to `/cards` to begin.
