import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/credit-cards/account/:accountId - Get credit cards by account
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
    
    const { searchParams } = new URL(request.url);
    const page = searchParams.get('page') || '0';
    const size = searchParams.get('size') || '20';
    const sort = searchParams.get('sort') || '';
    
    let queryString = `/api/v1/credit-cards/account/${params.accountId}?page=${page}&size=${size}`;
    if (sort) {
      queryString += `&sort=${sort}`;
    }
    
    const response = await forwardAuthRequest(queryString, 'GET', request);
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching credit cards by account:', error);
    return NextResponse.json({ error: 'Failed to fetch credit cards' }, { status: 500 });
  }
}
