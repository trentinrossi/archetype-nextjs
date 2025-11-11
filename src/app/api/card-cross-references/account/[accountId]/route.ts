import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/card-cross-references/account/:accountId - Get all card cross-references for an account
export async function GET(
  request: NextRequest,
  { params }: { params: { accountId: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/card-cross-references/account/${params.accountId}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching card cross-references by account:', error);
    return NextResponse.json({ error: 'Failed to fetch card cross-references' }, { status: 500 });
  }
}

// DELETE /api/card-cross-references/account/:accountId - Delete all card cross-references for an account
export async function DELETE(
  request: NextRequest,
  { params }: { params: { accountId: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/card-cross-references/account/${params.accountId}`,
      'DELETE',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json({ message: 'Card cross-references deleted successfully' }, { status: result.status });
  } catch (error) {
    console.error('Error deleting card cross-references:', error);
    return NextResponse.json({ error: 'Failed to delete card cross-references' }, { status: 500 });
  }
}
