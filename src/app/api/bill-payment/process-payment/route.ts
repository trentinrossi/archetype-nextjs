import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const { acctId } = body;

    if (!acctId || acctId.trim() === '') {
      return NextResponse.json(
        { 
          acctId: acctId || '',
          paymentAmount: 0,
          newBalance: 0,
          status: 'FAILED',
          message: 'Acct ID can NOT be empty...' 
        },
        { status: 400 }
      );
    }

    const response = await forwardAuthRequest(
      `/api/accounts/${acctId}/process-payment`,
      'POST',
      request
    );
    
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error: any) {
    console.error('Error processing payment:', error);
    
    if (error.message && error.message.includes('404')) {
      return NextResponse.json(
        { 
          paymentAmount: 0,
          newBalance: 0,
          status: 'FAILED',
          message: 'Account ID NOT found...' 
        },
        { status: 404 }
      );
    }
    
    if (error.message && error.message.includes('400')) {
      return NextResponse.json(
        { 
          paymentAmount: 0,
          newBalance: 0,
          status: 'FAILED',
          message: 'You have nothing to pay...' 
        },
        { status: 400 }
      );
    }
    
    return NextResponse.json(
      { 
        paymentAmount: 0,
        newBalance: 0,
        status: 'FAILED',
        message: 'Failed to process payment' 
      },
      { status: 500 }
    );
  }
}
