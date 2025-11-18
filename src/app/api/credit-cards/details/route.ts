import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const accountId = searchParams.get('accountId');
    const cardNumber = searchParams.get('cardNumber');
    
    if (!accountId || !cardNumber) {
      return NextResponse.json(
        { error: 'Both account ID and card number are required' },
        { status: 400 }
      );
    }
    
    const response = await forwardAuthRequest(
      `/api/credit-cards/details?accountId=${accountId}&cardNumber=${cardNumber}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching credit card details:', error);
    return NextResponse.json({ error: 'Failed to fetch credit card details' }, { status: 500 });
  }
}
