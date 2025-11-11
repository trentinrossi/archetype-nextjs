import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/transactions/account/:accountId - Get all transactions for an account
export async function GET(
  request: NextRequest,
  { params }: { params: { accountId: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/transactions/account/${params.accountId}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching transactions by account:', error);
    return NextResponse.json({ error: 'Failed to fetch transactions' }, { status: 500 });
  }
}
