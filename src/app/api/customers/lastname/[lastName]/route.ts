import { NextRequest, NextResponse } from 'next/server';

const BACKEND_API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8080';

export async function GET(
  request: NextRequest,
  { params }: { params: { lastName: string } }
) {
  try {
    const authHeader = request.headers.get('authorization');
    const { lastName } = params;

    const response = await fetch(`${BACKEND_API_URL}/api/customers/lastname/${lastName}`, {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json',
        ...(authHeader && { Authorization: authHeader }),
      },
    });

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({ error: 'Failed to fetch customers by last name' }));
      return NextResponse.json(errorData, { status: response.status });
    }

    const data = await response.json();
    return NextResponse.json(data, { status: 200 });
  } catch (error) {
    console.error('Error fetching customers by last name:', error);
    return NextResponse.json(
      { error: 'Failed to fetch customers by last name' },
      { status: 500 }
    );
  }
}
