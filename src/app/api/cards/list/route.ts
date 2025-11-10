import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/credit-cards/list - Get paginated list of cards with filtering
export async function GET(request: NextRequest) {
  try {
    const searchParams = request.nextUrl.searchParams;
    const accountId = searchParams.get('accountId');
    const cardNumber = searchParams.get('cardNumber');
    const page = searchParams.get('page') || '0';
    const size = searchParams.get('size') || '7';
    const sort = searchParams.get('sort') || '';

    let queryString = `page=${page}&size=${size}`;
    if (accountId) queryString += `&accountId=${accountId}`;
    if (cardNumber) queryString += `&cardNumber=${cardNumber}`;
    if (sort) queryString += `&sort=${sort}`;

    const response = await forwardAuthRequest(
      `/credit-cards/list?${queryString}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching cards list:', error);
    return NextResponse.json({ error: 'Failed to fetch cards list' }, { status: 500 });
  }
}
