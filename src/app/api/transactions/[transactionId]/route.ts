import { NextRequest, NextResponse } from 'next/server';
import { forwardAuthRequest, handleAuthApiResponse } from '@/lib/auth-middleware';

// GET /api/transactions/:transactionId - Get transaction by ID
export async function GET(
  request: NextRequest,
  { params }: { params: { transactionId: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/transactions/${params.transactionId}`,
      'GET',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json(result.data, { status: result.status });
  } catch (error) {
    console.error('Error fetching transaction:', error);
    return NextResponse.json({ error: 'Failed to fetch transaction' }, { status: 500 });
  }
}

// DELETE /api/transactions/:transactionId - Delete transaction
export async function DELETE(
  request: NextRequest,
  { params }: { params: { transactionId: string } }
) {
  try {
    const response = await forwardAuthRequest(
      `/api/transactions/${params.transactionId}`,
      'DELETE',
      request
    );
    const result = await handleAuthApiResponse(response);
    return NextResponse.json({ message: 'Transaction deleted successfully' }, { status: result.status });
  } catch (error) {
    console.error('Error deleting transaction:', error);
    return NextResponse.json({ error: 'Failed to delete transaction' }, { status: 500 });
  }
}
