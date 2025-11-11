import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/accounts/:accountId/balance - Get account balance
export async function GET(
  request: NextRequest,
  { params }: { params: { accountId: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/accounts/${params.accountId}/balance`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching account balance:', error);
    return NextResponse.json({ error: 'Failed to fetch account balance' }, { status: 500 });
  }
}
