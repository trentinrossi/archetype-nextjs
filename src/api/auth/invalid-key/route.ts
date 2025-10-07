import { NextRequest, NextResponse } from 'next/server';

// Handle Invalid Key Press - COSGN00C business logic
export async function POST(request: NextRequest) {
  try {
    // Invalid key press handling
    return NextResponse.json(
      {
        success: false,
        message: 'Invalid key pressed'
      },
      { status: 400 }
    );
  } catch (error) {
    console.error('Invalid key error:', error);
    return NextResponse.json(
      {
        success: false,
        message: 'Internal server error'
      },
      { status: 500 }
    );
  }
}