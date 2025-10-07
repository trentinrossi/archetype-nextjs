import { NextRequest, NextResponse } from 'next/server';

// Handle PF3 Exit - COSGN00C business logic
export async function POST(request: NextRequest) {
  try {
    // PF3 key press handling - graceful exit with thank you message
    return NextResponse.json(
      {
        success: true,
        message: 'Thank you for using CardDemo Application'
      },
      { status: 200 }
    );
  } catch (error) {
    console.error('Exit error:', error);
    return NextResponse.json(
      {
        success: false,
        message: 'Internal server error'
      },
      { status: 500 }
    );
  }
}