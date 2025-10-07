import { NextRequest, NextResponse } from 'next/server';
import { UserSecurityDTO, CreateUserSecurityRequest, UserListResponse, DEFAULT_PAGE_SIZE } from '@/types/user';

// Mock user database - in real implementation this would be a database
let mockUsers: UserSecurityDTO[] = [
  {
    userId: 'ADMIN001',
    firstName: 'System',
    lastName: 'Administrator',
    userType: 'A',
    createdAt: new Date().toISOString(),
    updatedAt: new Date().toISOString(),
  },
  {
    userId: 'USER001',
    firstName: 'General',
    lastName: 'User',
    userType: 'G',
    createdAt: new Date().toISOString(),
    updatedAt: new Date().toISOString(),
  },
];

// Mock password storage - in real implementation this would be hashed
const mockPasswords: Record<string, string> = {
  'ADMIN001': 'ADMIN123',
  'USER001': 'USER123',
};

function checkAuthentication(request: NextRequest) {
  const sessionCookie = request.cookies.get('user-session');
  
  if (!sessionCookie) {
    return null;
  }

  try {
    const sessionData = JSON.parse(sessionCookie.value);
    
    // Check if session is expired (30 minutes)
    const loginTime = new Date(sessionData.loginTime);
    const now = new Date();
    const diffMinutes = (now.getTime() - loginTime.getTime()) / (1000 * 60);
    
    if (diffMinutes > 30) {
      return null;
    }

    return sessionData;
  } catch {
    return null;
  }
}

// GET /api/users - List users with pagination and filtering (COUSR00C functionality)
export async function GET(request: NextRequest) {
  try {
    // Check authentication
    const session = checkAuthentication(request);
    if (!session) {
      return NextResponse.json(
        { message: 'Authentication required' },
        { status: 401 }
      );
    }

    const { searchParams } = new URL(request.url);
    const page = parseInt(searchParams.get('page') || '0');
    const pageSize = Math.min(parseInt(searchParams.get('pageSize') || DEFAULT_PAGE_SIZE.toString()), 100);
    const filter = searchParams.get('filter') || '';
    const sortBy = searchParams.get('sortBy') || 'userId';
    const sortOrder = searchParams.get('sortOrder') || 'asc';

    // Filter users based on search criteria
    let filteredUsers = mockUsers;
    
    if (filter) {
      const filterUpper = filter.toUpperCase();
      filteredUsers = mockUsers.filter(user => 
        user.userId.includes(filterUpper) ||
        user.firstName.toUpperCase().includes(filterUpper) ||
        user.lastName.toUpperCase().includes(filterUpper)
      );
    }

    // Sort users
    filteredUsers.sort((a, b) => {
      let aValue = a[sortBy as keyof UserSecurityDTO] as string;
      let bValue = b[sortBy as keyof UserSecurityDTO] as string;
      
      if (sortOrder === 'desc') {
        [aValue, bValue] = [bValue, aValue];
      }
      
      return aValue.localeCompare(bValue);
    });

    // Paginate
    const startIndex = page * pageSize;
    const endIndex = startIndex + pageSize;
    const paginatedUsers = filteredUsers.slice(startIndex, endIndex);

    const response: UserListResponse = {
      users: paginatedUsers,
      totalCount: filteredUsers.length,
      currentPage: page,
      totalPages: Math.ceil(filteredUsers.length / pageSize),
      pageSize,
    };

    return NextResponse.json(response, { status: 200 });

  } catch (error) {
    console.error('Get users error:', error);
    return NextResponse.json(
      { message: 'Internal server error' },
      { status: 500 }
    );
  }
}

// POST /api/users - Create new user (COUSR01C functionality)
export async function POST(request: NextRequest) {
  try {
    // Check authentication
    const session = checkAuthentication(request);
    if (!session) {
      return NextResponse.json(
        { message: 'Authentication required' },
        { status: 401 }
      );
    }

    // Check if user is admin
    if (session.userType !== 'A') {
      return NextResponse.json(
        { message: 'Administrator access required' },
        { status: 403 }
      );
    }

    const body: CreateUserSecurityRequest = await request.json();

    // Validate input (COBOL validation rules)
    const errors: string[] = [];

    if (!body.userId || body.userId.trim() === '') {
      errors.push('User ID can NOT be empty...');
    } else if (body.userId.length > 8) {
      errors.push('User ID must be 8 characters or less');
    }

    if (!body.firstName || body.firstName.trim() === '') {
      errors.push('First Name can NOT be empty...');
    } else if (body.firstName.length > 20) {
      errors.push('First Name must be 20 characters or less');
    }

    if (!body.lastName || body.lastName.trim() === '') {
      errors.push('Last Name can NOT be empty...');
    } else if (body.lastName.length > 20) {
      errors.push('Last Name must be 20 characters or less');
    }

    if (!body.password || body.password.trim() === '') {
      errors.push('Password can NOT be empty...');
    } else if (body.password.length > 8) {
      errors.push('Password must be 8 characters or less');
    }

    if (!body.userType || (body.userType !== 'A' && body.userType !== 'G')) {
      errors.push('User Type can NOT be empty...');
    }

    if (errors.length > 0) {
      return NextResponse.json(
        { message: errors[0], errors },
        { status: 400 }
      );
    }

    // Convert to uppercase for consistency (COBOL behavior)
    const userId = body.userId.toUpperCase().trim();

    // Check if user already exists
    if (mockUsers.find(u => u.userId === userId)) {
      return NextResponse.json(
        { message: 'User ID already exist...' },
        { status: 409 }
      );
    }

    // Create new user
    const newUser: UserSecurityDTO = {
      userId,
      firstName: body.firstName.trim(),
      lastName: body.lastName.trim(),
      userType: body.userType,
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString(),
    };

    mockUsers.push(newUser);
    mockPasswords[userId] = body.password;

    return NextResponse.json(
      {
        message: `User ${userId} has been added...`,
        user: newUser,
      },
      { status: 201 }
    );

  } catch (error) {
    console.error('Create user error:', error);
    return NextResponse.json(
      { message: 'Unable to Add User...' },
      { status: 500 }
    );
  }
}