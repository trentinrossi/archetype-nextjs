import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/transactions/bill-payments/account/:accountId - Get bill payment transactions for an account
export async function GET(
  request: NextRequest,
  { params }: { params: { accountId: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/transactions/bill-payments/account/${params.accountId}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching bill payment transactions by account:', error);
    return NextResponse.json({ error: 'Failed to fetch bill payment transactions' }, { status: 500 });
  }
}
