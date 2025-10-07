'use client';

import React from 'react';
import { useRouter } from 'next/navigation';
import { UserAddForm } from '@/components/UserAddForm';

export default function UserAddPage() {
  const router = useRouter();

  // COUSR01C business rule: Handle successful user creation
  const handleSuccess = (message: string) => {
    console.log('User added successfully:', message);
    // Could show a toast notification here
    // Form is automatically cleared by the component after success
  };

  // Handle exit (PF3) - return to admin menu
  const handleExit = () => {
    router.push('/admin');
  };

  return (
    <UserAddForm
      onSuccess={handleSuccess}
      onExit={handleExit}
    />
  );
}