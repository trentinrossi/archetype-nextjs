import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// POST /api/credit-cards/filter - Filter credit cards based on user permissions and criteria
export async function POST(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const userId = searchParams.get('userId');
    const page = searchParams.get('page') || '0';
    const size = searchParams.get('size') || '20';
    
    if (!userId) {
      return NextResponse.json(
        { error: 'userId is required' },
        { status: 400 }
      );
    }
    
    const body = await request.json();
    
    const response = await forwardAuthRequest(
      `/api/credit-cards/filter?userId=${userId}&page=${page}&size=${size}`,
      'POST',
      request,
      body
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error filtering credit cards:', error);
    return NextResponse.json(
      { error: 'Failed to filter credit cards' },
      { status: 500 }
    );
  }
}
