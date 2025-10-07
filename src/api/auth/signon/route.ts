import { NextRequest, NextResponse } from 'next/server';
import { SignonRequestDTO, SignonResponseDTO } from '@/types/user';

// Mock user database - in real implementation, this would be a database
const mockUsers = [
  {
    userId: 'ADMIN001',
    password: 'ADMIN123',
    userType: 'ADMIN' as const,
    active: true,
    firstName: 'System',
    lastName: 'Administrator'
  },
  {
    userId: 'USER001',
    password: 'USER123',
    userType: 'GENERAL' as const,
    active: true,
    firstName: 'General',
    lastName: 'User'
  }
];

export async function POST(request: NextRequest) {
  try {
    const body: SignonRequestDTO = await request.json();
    
    // Validate input - COSGN00C business logic
    if (!body.userId || body.userId.trim() === '') {
      return NextResponse.json(
        { 
          success: false, 
          message: 'Please enter User ID',
          userType: 'GENERAL',
          redirectProgram: ''
        } as SignonResponseDTO,
        { status: 400 }
      );
    }

    if (!body.password || body.password.trim() === '') {
      return NextResponse.json(
        { 
          success: false, 
          message: 'Please enter Password',
          userType: 'GENERAL',
          redirectProgram: ''
        } as SignonResponseDTO,
        { status: 400 }
      );
    }

    // Validate field lengths
    if (body.userId.length > 8) {
      return NextResponse.json(
        { 
          success: false, 
          message: 'User ID must be 8 characters or less',
          userType: 'GENERAL',
          redirectProgram: ''
        } as SignonResponseDTO,
        { status: 400 }
      );
    }

    if (body.password.length > 8) {
      return NextResponse.json(
        { 
          success: false, 
          message: 'Password must be 8 characters or less',
          userType: 'GENERAL',
          redirectProgram: ''
        } as SignonResponseDTO,
        { status: 400 }
      );
    }

    // Convert to uppercase for case-insensitive lookup
    const userIdUpper = body.userId.toUpperCase();
    
    // Find user in mock database
    const user = mockUsers.find(u => u.userId === userIdUpper);
    
    if (!user) {
      return NextResponse.json(
        { 
          success: false, 
          message: 'User not found',
          userType: 'GENERAL',
          redirectProgram: ''
        } as SignonResponseDTO,
        { status: 401 }
      );
    }

    // Check if user is active
    if (!user.active) {
      return NextResponse.json(
        { 
          success: false, 
          message: 'User account is inactive',
          userType: 'GENERAL',
          redirectProgram: ''
        } as SignonResponseDTO,
        { status: 403 }
      );
    }

    // Validate password
    if (user.password !== body.password) {
      return NextResponse.json(
        { 
          success: false, 
          message: 'Wrong Password',
          userType: 'GENERAL',
          redirectProgram: ''
        } as SignonResponseDTO,
        { status: 401 }
      );
    }

    // Successful authentication - determine redirect program based on user type
    const redirectProgram = user.userType === 'ADMIN' ? 'COADM01C' : 'COMEN01C';
    
    return NextResponse.json(
      {
        success: true,
        message: 'Authentication successful',
        userType: user.userType,
        redirectProgram: redirectProgram
      } as SignonResponseDTO,
      { status: 200 }
    );

  } catch (error) {
    console.error('Signon error:', error);
    return NextResponse.json(
      { 
        success: false, 
        message: 'Internal server error',
        userType: 'GENERAL',
        redirectProgram: ''
      } as SignonResponseDTO,
      { status: 500 }
    );
  }
}