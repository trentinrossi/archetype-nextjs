import { NextRequest, NextResponse } from 'next/server';

const BACKEND_API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8080';

export async function POST(request: NextRequest) {
  try {
    const authHeader = request.headers.get('authorization');
    const { searchParams } = new URL(request.url);
    const processingDate = searchParams.get('processingDate');
    
    const url = processingDate 
      ? `${BACKEND_API_URL}/api/interest/calculate?processingDate=${processingDate}`
      : `${BACKEND_API_URL}/api/interest/calculate`;

    const response = await fetch(url, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        ...(authHeader && { Authorization: authHeader }),
      },
    });

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({ error: 'Failed to calculate interest' }));
      return NextResponse.json(errorData, { status: response.status });
    }

    const data = await response.json();
    return NextResponse.json(data, { status: 200 });
  } catch (error) {
    console.error('Error calculating interest:', error);
    return NextResponse.json(
      { error: 'Failed to calculate interest' },
      { status: 500 }
    );
  }
}
