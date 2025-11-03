import { NextRequest, NextResponse } from 'next/server';

const BACKEND_API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8080';

export async function GET(
  request: NextRequest,
  { params }: { params: { cardNumber: string } }
) {
  try {
    const authHeader = request.headers.get('authorization');
    const { cardNumber } = params;

    const response = await fetch(`${BACKEND_API_URL}/api/transactions/card/${cardNumber}`, {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json',
        ...(authHeader && { Authorization: authHeader }),
      },
    });

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({ error: 'Failed to fetch transactions' }));
      return NextResponse.json(errorData, { status: response.status });
    }

    const data = await response.json();
    return NextResponse.json(data, { status: 200 });
  } catch (error) {
    console.error('Error fetching transactions by card:', error);
    return NextResponse.json(
      { error: 'Failed to fetch transactions by card' },
      { status: 500 }
    );
  }
}
