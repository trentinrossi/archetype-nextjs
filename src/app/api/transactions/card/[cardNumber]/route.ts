import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/transactions/card/:cardNumber - Get all transactions for a card
export async function GET(
  request: NextRequest,
  { params }: { params: { cardNumber: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/transactions/card/${params.cardNumber}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching transactions by card:', error);
    return NextResponse.json({ error: 'Failed to fetch transactions' }, { status: 500 });
  }
}
