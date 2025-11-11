import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function POST(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  try {
    const { searchParams } = new URL(request.url);
    const reenterFlag = searchParams.get('reenterFlag');
    
    const response = await forwardAuthRequest(
      `/api/user-sessions/${params.id}/set-reenter-flag?reenterFlag=${reenterFlag}`,
      'POST',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error setting reenter flag:', error);
    return NextResponse.json({ error: 'Failed to set reenter flag' }, { status: 500 });
  }
}
