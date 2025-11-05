# CardDemo - Quick Start Guide

## 🚀 Getting Started in 5 Minutes

### Prerequisites
- Node.js 18+ installed
- npm or yarn package manager
- Backend API running (see API Configuration below)

### Installation

1. **Install Dependencies**
```bash
npm install
```

2. **Configure API Endpoint**
Edit the API_BASE_URL in service files if needed:
- `src/services/userService.ts`
- `src/services/transactionService.ts`

Default: `/api` (relative path)

3. **Run Development Server**
```bash
npm run dev
```

4. **Open Application**
Navigate to [http://localhost:3000](http://localhost:3000)

---

## 🔐 Login Credentials

The application requires backend authentication. Use credentials provided by your system administrator.

**Example Login Flow:**
1. Enter User ID (max 8 characters)
2. Enter Password (max 8 characters)
3. Click "Sign In"
4. Redirected based on user type:
   - Admin (A) → `/admin`
   - Regular (R) → `/main`

---

## 📱 Application Structure

### For Administrators (User Type: A)

**Admin Menu** (`/admin`)
```
1. User List       → /admin/users
2. User Add        → /admin/users/add
3. User Update     → /admin/users/update
4. User Delete     → /admin/users/delete
5. Transaction Reports → /admin/reports
```

### For Regular Users (User Type: R)

**Main Menu** (`/main`)
```
1. View Transactions → /main/transactions
2. Account Information → /main/account
```

---

## 🎯 Key Features

### User Management (Admin Only)
- **List Users**: View all users with pagination (10 per page)
- **Add User**: Create new user accounts
- **Update User**: Modify existing user information
- **Delete User**: Remove user accounts (with confirmation)

### Transaction Reports (Admin Only)
- **Monthly Report**: Current month transactions
- **Yearly Report**: Current year transactions
- **Custom Report**: Specify date range (yyyy-MM-dd)

### Transaction Viewing (All Users)
- View paginated transaction list
- Search by Transaction ID
- See formatted dates and amounts

---

## ⌨️ Keyboard Shortcuts (Function Keys)

| Key | Action | Available On |
|-----|--------|--------------|
| **PF3** | Return to previous menu / Sign out | All pages |
| **PF4** | Clear current screen | Add/Update forms |
| **PF5** | Save changes / Confirm action | Update/Delete pages |
| **PF7** | Previous page | List pages |
| **PF8** | Next page | List pages |
| **PF12** | Return to admin menu | Admin pages |

*Note: All function keys are also available as clickable buttons*

---

## 📋 Common Tasks

### Adding a New User (Admin)
1. Go to Admin Menu → User Add
2. Fill in all required fields:
   - User ID (max 8 chars)
   - First Name (max 20 chars)
   - Last Name (max 20 chars)
   - Password (max 8 chars)
   - User Type (Admin or Regular)
3. Click "Create User"
4. Success message appears
5. Redirected to User List

### Updating a User (Admin)
1. Go to Admin Menu → User Update
2. Enter User ID to search
3. Click "Search"
4. Modify desired fields
5. Click "Update User" or press PF5
6. Success message appears

### Deleting a User (Admin)
1. Go to Admin Menu → User Delete
2. Enter User ID to search
3. Click "Search"
4. Review user information
5. Click "PF5 - Delete User"
6. Confirm deletion in dialog
7. Success message appears

### Generating Reports (Admin)
1. Go to Admin Menu → Transaction Reports
2. Choose report type:
   - **Monthly**: Click "Monthly Report"
   - **Yearly**: Click "Yearly Report"
   - **Custom**: Enter start/end dates, click "Generate Custom Report"
3. View results in transaction list

### Viewing Transactions (All Users)
1. Go to Main Menu → View Transactions
2. Browse paginated list
3. Use PF7/PF8 or buttons to navigate pages
4. Optional: Search by Transaction ID

---

## 🎨 UI Components

### Color Coding
- **Indigo/Blue**: Primary actions, headers
- **Green**: Success messages
- **Red**: Error messages, delete actions
- **Purple**: Admin user type
- **Blue**: Regular user type
- **Gray**: Secondary actions, disabled states

### Icons
- 🔐 Lock: Authentication
- ⚙️ Gear: Admin functions
- 👤 User: User management
- 📊 Chart: Reports
- 📋 Clipboard: Transactions
- 🏠 Home: Main menu

---

## 🔧 Troubleshooting

### Login Issues
- **"Wrong Password"**: Verify credentials with admin
- **"User not found"**: Check User ID spelling
- **Redirect loop**: Clear browser cache and cookies

### API Errors
- **"Failed to fetch"**: Check backend API is running
- **"Network error"**: Verify API_BASE_URL configuration
- **401 Unauthorized**: Session expired, login again

### Display Issues
- **Blank screen**: Check browser console for errors
- **Styling broken**: Clear cache, reload page
- **Slow loading**: Check network connection

---

## 📊 Data Formats

### Dates
- **Input**: yyyy-MM-dd (e.g., 2024-01-15)
- **Display**: MM/dd/yy (e.g., 01/15/24)

### Amounts
- **Display**: $X,XXX.XX (e.g., $1,234.56)
- **Precision**: 2 decimal places

### User Types
- **A**: Administrator (full access)
- **R**: Regular user (limited access)

---

## 🌐 Browser Support

✅ Chrome (latest)
✅ Firefox (latest)
✅ Safari (latest)
✅ Edge (latest)

---

## 📱 Mobile Support

The application is fully responsive and works on:
- 📱 Smartphones (iOS, Android)
- 📱 Tablets (iPad, Android tablets)
- 💻 Desktop computers
- 🖥️ Large displays

---

## 🆘 Getting Help

### Documentation
- **README_CARDDEMO.md**: Complete application documentation
- **IMPLEMENTATION_SUMMARY.md**: Technical implementation details

### Support
Contact your system administrator for:
- Login credentials
- User account issues
- API configuration
- Technical support

---

## 🚦 Quick Reference

### Admin Workflow
```
Login → Admin Menu → Select Function → Perform Action → Return to Menu
```

### User Workflow
```
Login → Main Menu → View Transactions/Account → Return to Menu
```

### Navigation Pattern
```
All Pages: PF3 = Back/Sign Out
List Pages: PF7 = Previous, PF8 = Next
Forms: PF4 = Clear, PF5 = Save
```

---

## ✅ Checklist for First Use

- [ ] Backend API is running
- [ ] API endpoint configured correctly
- [ ] Dependencies installed (`npm install`)
- [ ] Development server started (`npm run dev`)
- [ ] Browser opened to localhost:3000
- [ ] Login credentials obtained
- [ ] Successfully logged in
- [ ] Navigated to appropriate menu (Admin/Main)
- [ ] Tested basic functionality

---

## 🎓 Learning Path

1. **Start Here**: Login and explore menus
2. **Admin Users**: Try User List → Add User → Update User
3. **All Users**: View Transactions, check Account Info
4. **Advanced**: Generate reports, use search features
5. **Power User**: Learn keyboard shortcuts (PF keys)

---

**Ready to go!** 🎉

Start the development server and navigate to the login page to begin using CardDemo.

For detailed information, see **README_CARDDEMO.md**
