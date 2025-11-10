import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/credit-cards/account/:accountId/count - Get count of credit cards for account
export async function GET(
  request: NextRequest,
  { params }: { params: { accountId: string } }
) {
  try {
    // Validate account ID (11 digits)
    if (!/^\d{11}$/.test(params.accountId)) {
      return NextResponse.json(
        { error: 'Account ID must be exactly 11 digits' },
        { status: 400 }
      );
    }
    
    const response = await forwardAuthRequest(
      `/api/v1/credit-cards/account/${params.accountId}/count`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error getting card count:', error);
    return NextResponse.json({ error: 'Failed to get card count' }, { status: 500 });
  }
}
