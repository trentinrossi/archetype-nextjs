'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { transactionService } from '@/services/transactionService';
import { Account, BillPaymentResponse } from '@/types/cardServices';
import { Input, Button, Modal } from '@/components/ui';

export default function BillPaymentPage() {
  const router = useRouter();
  const [accountId, setAccountId] = useState('');
  const [account, setAccount] = useState<Account | null>(null);
  const [loading, setLoading] = useState(false);
  const [processing, setProcessing] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [showConfirmModal, setShowConfirmModal] = useState(false);
  const [paymentResult, setPaymentResult] = useState<BillPaymentResponse | null>(null);
  const [showSuccessModal, setShowSuccessModal] = useState(false);

  const handleLookupAccount = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!accountId || accountId.length !== 11) {
      setError('Account ID must be exactly 11 digits');
      return;
    }

    try {
      setLoading(true);
      setError(null);
      setAccount(null);

      const data = await accountService.getAccountById(accountId);
      
      if (data.currentBalance <= 0) {
        setError('This account has no outstanding balance to pay.');
        setAccount(null);
        return;
      }

      setAccount(data);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to lookup account');
      setAccount(null);
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handlePaymentConfirmation = () => {
    if (!account) return;
    setShowConfirmModal(true);
  };

  const handleProcessPayment = async () => {
    if (!account) return;

    try {
      setProcessing(true);
      setShowConfirmModal(false);

      const result = await transactionService.processBillPayment({
        accountId: account.accountId,
        confirmPayment: 'Y',
      });

      setPaymentResult(result);
      setShowSuccessModal(true);
      setAccount(null);
      setAccountId('');
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to process payment');
      console.error(err);
    } finally {
      setProcessing(false);
    }
  };

  const handleNewPayment = () => {
    setPaymentResult(null);
    setShowSuccessModal(false);
    setAccount(null);
    setAccountId('');
    setError(null);
  };

  const formatCurrency = (amount: number): string => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  const formatDate = (dateString: string): string => {
    return new Date(dateString).toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'long',
      day: 'numeric',
    });
  };

  return (
    <div className="p-6 max-w-4xl mx-auto">
      <div className="mb-6">
        <h1 className="text-2xl font-bold text-gray-900">Bill Payment</h1>
        <p className="text-sm text-gray-600 mt-1">Process full bill payments for credit card accounts</p>
      </div>

      <div className="bg-white rounded-lg shadow p-6 mb-6">
        <h2 className="text-lg font-semibold text-gray-900 mb-4">Account Lookup</h2>
        <form onSubmit={handleLookupAccount} className="space-y-4">
          <Input
            label="Account ID"
            placeholder="Enter 11-digit account ID"
            value={accountId}
            onChange={(e) => {
              const value = e.target.value.replace(/\D/g, '').slice(0, 11);
              setAccountId(value);
              setError(null);
            }}
            required
            maxLength={11}
          />
          <div className="flex gap-2">
            <Button type="submit" disabled={loading || accountId.length !== 11}>
              {loading ? 'Looking up...' : 'Lookup Account'}
            </Button>
            {account && (
              <Button
                type="button"
                variant="secondary"
                onClick={() => {
                  setAccount(null);
                  setAccountId('');
                  setError(null);
                }}
              >
                Clear
              </Button>
            )}
          </div>
        </form>
      </div>

      {error && (
        <div className="bg-red-50 border border-red-200 rounded-lg p-4 mb-6">
          <div className="flex items-center">
            <span className="text-red-600 text-xl mr-3">⚠️</span>
            <div>
              <h3 className="text-red-800 font-semibold">Error</h3>
              <p className="text-red-600">{error}</p>
            </div>
          </div>
        </div>
      )}

      {account && (
        <div className="bg-white rounded-lg shadow p-6 mb-6">
          <h2 className="text-lg font-semibold text-gray-900 mb-4">Account Details</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Account ID</label>
              <p className="text-gray-900 font-mono">{account.accountId}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Status</label>
              <span
                className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${
                  account.activeStatus === 'Y'
                    ? 'bg-green-100 text-green-800'
                    : 'bg-red-100 text-red-800'
                }`}
              >
                {account.activeStatus === 'Y' ? 'Active' : 'Inactive'}
              </span>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Current Balance</label>
              <p className="text-2xl font-bold text-red-600">{formatCurrency(account.currentBalance)}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Credit Limit</label>
              <p className="text-gray-900">{formatCurrency(account.creditLimit)}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Available Credit</label>
              <p className="text-gray-900">{formatCurrency(account.availableCredit)}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Open Date</label>
              <p className="text-gray-900">{formatDate(account.openDate)}</p>
            </div>
          </div>

          <div className="mt-6 pt-6 border-t border-gray-200">
            <div className="bg-blue-50 border border-blue-200 rounded-lg p-4 mb-4">
              <div className="flex items-start">
                <span className="text-blue-600 text-xl mr-3">ℹ️</span>
                <div>
                  <h4 className="text-blue-800 font-semibold mb-1">Payment Information</h4>
                  <p className="text-blue-700 text-sm">
                    This will process a full payment of <strong>{formatCurrency(account.currentBalance)}</strong> for account <strong>{account.accountId}</strong>.
                    The new balance will be <strong>$0.00</strong> after payment.
                  </p>
                </div>
              </div>
            </div>
            <Button onClick={handlePaymentConfirmation} disabled={processing}>
              Process Payment
            </Button>
          </div>
        </div>
      )}

      {showConfirmModal && account && (
        <Modal
          isOpen={showConfirmModal}
          onClose={() => setShowConfirmModal(false)}
          title="Confirm Bill Payment"
        >
          <div className="space-y-4">
            <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-4">
              <div className="flex items-start">
                <span className="text-yellow-600 text-xl mr-3">⚠️</span>
                <div>
                  <h4 className="text-yellow-800 font-semibold mb-1">Confirm Payment</h4>
                  <p className="text-yellow-700 text-sm">
                    Please review the payment details carefully before confirming.
                  </p>
                </div>
              </div>
            </div>

            <div className="space-y-3">
              <div className="flex justify-between py-2 border-b border-gray-200">
                <span className="text-gray-600">Account ID:</span>
                <span className="font-mono font-semibold">{account.accountId}</span>
              </div>
              <div className="flex justify-between py-2 border-b border-gray-200">
                <span className="text-gray-600">Current Balance:</span>
                <span className="font-semibold text-red-600">{formatCurrency(account.currentBalance)}</span>
              </div>
              <div className="flex justify-between py-2 border-b border-gray-200">
                <span className="text-gray-600">Payment Amount:</span>
                <span className="font-semibold text-green-600">{formatCurrency(account.currentBalance)}</span>
              </div>
              <div className="flex justify-between py-2 border-b border-gray-200">
                <span className="text-gray-600">New Balance:</span>
                <span className="font-semibold text-green-600">$0.00</span>
              </div>
            </div>

            <div className="flex gap-2 pt-4">
              <Button onClick={handleProcessPayment} disabled={processing}>
                {processing ? 'Processing...' : 'Confirm Payment'}
              </Button>
              <Button
                variant="secondary"
                onClick={() => setShowConfirmModal(false)}
                disabled={processing}
              >
                Cancel
              </Button>
            </div>
          </div>
        </Modal>
      )}

      {showSuccessModal && paymentResult && (
        <Modal
          isOpen={showSuccessModal}
          onClose={() => setShowSuccessModal(false)}
          title="Payment Successful"
        >
          <div className="space-y-4">
            <div className="bg-green-50 border border-green-200 rounded-lg p-4">
              <div className="flex items-start">
                <span className="text-green-600 text-xl mr-3">✓</span>
                <div>
                  <h4 className="text-green-800 font-semibold mb-1">Payment Processed Successfully</h4>
                  <p className="text-green-700 text-sm">
                    Your bill payment has been processed and the account balance has been updated.
                  </p>
                </div>
              </div>
            </div>

            <div className="space-y-3">
              <div className="flex justify-between py-2 border-b border-gray-200">
                <span className="text-gray-600">Transaction ID:</span>
                <span className="font-mono font-semibold">{paymentResult.transactionId}</span>
              </div>
              <div className="flex justify-between py-2 border-b border-gray-200">
                <span className="text-gray-600">Account ID:</span>
                <span className="font-mono font-semibold">{paymentResult.accountId}</span>
              </div>
              <div className="flex justify-between py-2 border-b border-gray-200">
                <span className="text-gray-600">Payment Amount:</span>
                <span className="font-semibold text-green-600">{formatCurrency(paymentResult.paymentAmount)}</span>
              </div>
              <div className="flex justify-between py-2 border-b border-gray-200">
                <span className="text-gray-600">New Balance:</span>
                <span className="font-semibold text-green-600">{formatCurrency(paymentResult.currentBalance)}</span>
              </div>
            </div>

            <div className="flex gap-2 pt-4">
              <Button onClick={handleNewPayment}>
                Process Another Payment
              </Button>
              <Button
                variant="secondary"
                onClick={() => router.push('/accounts')}
              >
                View Accounts
              </Button>
            </div>
          </div>
        </Modal>
      )}
    </div>
  );
}
