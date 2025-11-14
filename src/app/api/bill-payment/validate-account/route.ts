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
          valid: false,
          message: 'Acct ID can NOT be empty...' 
        },
        { status: 400 }
      );
    }

    const response = await forwardAuthRequest(
      `/api/accounts/${acctId}/validate`,
      'POST',
      request
    );
    
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error: any) {
    console.error('Error validating account:', error);
    
    if (error.message && error.message.includes('404')) {
      return NextResponse.json(
        { 
          valid: false,
          message: 'Account ID NOT found...' 
        },
        { status: 404 }
      );
    }
    
    return NextResponse.json(
      { 
        valid: false,
        message: 'Failed to validate account' 
      },
      { status: 500 }
    );
  }
}
