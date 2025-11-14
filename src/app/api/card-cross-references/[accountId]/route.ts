import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

export async function GET(
  request: NextRequest,
  { params }: { params: { accountId: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/card-cross-references/${params.accountId}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching card cross reference:', error);
    return NextResponse.json({ error: 'Failed to fetch card cross reference' }, { status: 500 });
  }
}

export async function PUT(
  request: NextRequest,
  { params }: { params: { accountId: string } }
) {
  try {
    const body = await request.json();
    const response = await forwardAuthRequest(
      `/api/card-cross-references/${params.accountId}`,
      'PUT',
      request,
      body
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error updating card cross reference:', error);
    return NextResponse.json({ error: 'Failed to update card cross reference' }, { status: 500 });
  }
}

export async function DELETE(
  request: NextRequest,
  { params }: { params: { accountId: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/card-cross-references/${params.accountId}`,
      'DELETE',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json({ message: 'Card cross reference deleted successfully' }, { status: result.status });
  } catch (error) {
    console.error('Error deleting card cross reference:', error);
    return NextResponse.json({ error: 'Failed to delete card cross reference' }, { status: 500 });
  }
}
